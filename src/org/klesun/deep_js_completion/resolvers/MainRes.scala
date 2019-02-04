package org.klesun.deep_js_completion.resolvers

import java.util

import com.intellij.lang.javascript.psi.JSRecordType.TypeMember
import com.intellij.lang.javascript.psi._
import com.intellij.lang.javascript.psi.ecmal4.JSAttributeList
import com.intellij.lang.javascript.psi.ecmal4.JSAttributeList.ModifierType
import com.intellij.lang.javascript.psi.impl.JSLiteralExpressionImpl
import com.intellij.lang.javascript.psi.types._
import com.intellij.psi.PsiElement
import org.klesun.deep_js_completion.contexts.IExprCtx
import org.klesun.deep_js_completion.helpers.Mt
import org.klesun.deep_js_completion.structures.{JSDeepClassType, JSDeepFunctionTypeImpl}
import org.klesun.lang.Lang._

import scala.collection.GenTraversableOnce
import scala.collection.JavaConverters._
import scala.util.Try

object MainRes {

  def getReturns(func: PsiElement): Iterable[JSExpression] = {
    val arrow = cast[JSFunctionExpression](func)
      .flatMap(f => Option(f.getLastChild))
      .flatMap(cast[JSExpression](_))
    val classic = func.getChildren.itr
      .filter(c => !c.isInstanceOf[JSFunction])
      .flatMap(c => getReturns(c) ++ cast[JSReturnStatement](c)
        .flatMap(ret => Option(ret.getExpression)))
    arrow.++(classic)
  }

  def resolveIn(expr: JSExpression, ctx: IExprCtx): GenTraversableOnce[JSType] = {
    val types: GenTraversableOnce[JSType] = expr match {
      case newex: JSNewExpression =>
        if (Option(newex.getMethodExpression).forall(ref => ref.getText equals "Promise")) {
          val types = newex.getArguments.lift(0)
            .flatMap(cbArg => cast[JSFunction](cbArg))
            .flatMap(f => f.getParameters.lift(0))
            .flatMap(cbArg => cast[JSParameter](cbArg))
            .itr.flatMap(par => VarRes.findVarUsages(par, par.getName))
            .flatMap(usage => Option(usage.getParent)
              .flatMap(cast[JSCallExpression](_))
              .filter(call => usage eq call.getMethodExpression))
            .flatMap(call => call.getArguments.lift(0))
            .flatMap(value => ctx.subCtxEmpty().findExprType(value))
            .map(valuet => Mt.wrapPromise(valuet))
          types
        } else {
          nit(newex.getMethodExpression)
            .flatMap(exp => ctx.findExprType(exp)).itr
            .flatMap(Mt.flattenTypes)
            .flatMap(cast[JSDeepClassType](_))
            .flatMap(clst => clst.getNewInstType(ctx.subCtxDirect(newex)))
        }
      case call: JSCallExpression => FuncCallRes(ctx).resolve(call)
      case vari: JSReferenceExpression => VarRes(ctx).resolve(vari)
      case indx: JSIndexedPropertyAccessExpression =>
        nit(indx.getQualifier)
          .flatMap(qual => ctx.findExprType(qual))
          .flatMap(arrT => {
            val keyTOpt = nit(indx.getIndexExpression)
              .flatMap(qua => ctx.findExprType(qua))
            val result = Mt.getKey(arrT, keyTOpt)
            result
          })
      case func: JSFunctionExpression =>
        val returns = getReturns(func)
        Some(JSDeepFunctionTypeImpl(func, ctx.func(), callCtx =>
          returns.flatMap(r => {
            /** @debug */
            val exc = new RuntimeException("stack too long, fucking hamburger " + func.getName + " " + callCtx)
            if (exc.getStackTrace.length > 1000) {
              throw exc
            }
            val isAsync = func.getChildren.flatMap(cast[JSAttributeList](_))
              .exists(lst => lst.hasModifier(ModifierType.ASYNC))
            val rett = callCtx.findExprType(r)
            if (!isAsync) rett else {
              rett.itr.map(t => Mt.wrapPromise(t))
            }
          }))
        )
      case arr: JSArrayLiteralExpression =>
        val typeTuple = arr.getExpressions
          .flatMap(el => frs(ctx.findExprType(el), Some(JSUnknownType.JS_INSTANCE)))
          .toList.asJava
        Some(new JSTupleTypeImpl(JSTypeSource.EMPTY, typeTuple, true, -1))
      case obje: JSObjectLiteralExpression =>
        val props: util.List[TypeMember] = obje.getProperties.flatMap(p => {
          val getValue = () => nit(p.getValue)
            .flatMap(expr => ctx.findExprType(expr))
          Option(p.getName).map(n => Mt.mkProp(n, () => getValue(), Some(p)))
        }).toList.asJava
        Some(new JSRecordTypeImpl(JSTypeSource.EMPTY, props))
      case bina: JSBinaryExpression =>
        val types = (Option(bina.getLOperand) ++ Option(bina.getROperand))
          .flatMap(op => ctx.findExprType(op))
        types
      case lit: JSLiteralExpressionImpl =>
        if (lit.isBooleanLiteral) {
          Some(new JSBooleanLiteralTypeImpl(lit.getValue.asInstanceOf[Boolean], false, JSTypeSource.EMPTY))
        } else if (lit.isNumericLiteral) {
          Try((lit.getValue + "").toDouble).toOption
            .map(valu => new JSNumberLiteralTypeImpl(valu, false, JSTypeSource.EMPTY, lit.getValue + ""))
        } else {
          None
        }
      case tern: JSConditionalExpression =>
        List(tern.getThen, tern.getElse)
          .flatMap(expr => ctx.findExprType(expr))
      case par: JSParenthesizedExpression =>
        nit(par.getInnerExpression)
          .flatMap(inner => ctx.findExprType(inner))
      case pref: JSPrefixExpression =>
        if ("JS:AWAIT_KEYWORD" equals (pref.getOperationSign + "")) {
          nit(pref.getExpression)
            .flatMap(expr => ctx.findExprType(expr))
            .flatMap(pomiset => Mt.unwrapPromise(pomiset))
        } else {
          None
        }
      case _ => {
        None
      }
    }
    types
  }
}
