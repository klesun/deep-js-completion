package org.klesun.deep_js_completion.resolvers

import java.util

import com.intellij.lang.javascript.JSKeywordElementType
import com.intellij.lang.javascript.psi.JSRecordType.TypeMember
import com.intellij.lang.javascript.psi._
import com.intellij.lang.javascript.psi.impl.JSLiteralExpressionImpl
import com.intellij.lang.javascript.psi.resolve.JSTypeEvaluator
import com.intellij.lang.javascript.psi.types.JSRecordMemberSourceFactory.EmptyMemberSource
import com.intellij.lang.javascript.psi.types.JSRecordTypeImpl.PropertySignatureImpl
import com.intellij.lang.javascript.psi.types._
import com.intellij.psi.PsiElement
import org.klesun.deep_js_completion.contexts.IExprCtx
import org.klesun.deep_js_completion.helpers.Mt
import org.klesun.deep_js_completion.structures.JSDeepFunctionTypeImpl
import org.klesun.lang.Lang._

import scala.collection.JavaConverters._
import scala.util.Try

object MainRes {

  def getReturns(func: PsiElement): List[JSExpression] = {
    val arrow = cast[JSFunctionExpression](func)
      .flatMap(f => Option(f.getLastChild))
      .flatMap(cast[JSExpression](_))
    val classic = func.getChildren.toList
      .filter(c => !c.isInstanceOf[JSFunction])
      .flatMap(c => getReturns(c) ++ cast[JSReturnStatement](c)
        .flatMap(ret => Option(ret.getExpression)))
    arrow.++(classic).toList
  }

  def resolveIn(expr: JSExpression, ctx: IExprCtx): Option[JSType] = {
    expr match {
      case newex: JSNewExpression =>
        if (Option(newex.getMethodExpression).forall(ref => ref.getText equals "Promise")) {
          val types = newex.getArguments.lift(0)
            .flatMap(cbArg => cast[JSFunction](cbArg))
            .flatMap(f => f.getParameters.lift(0))
            .flatMap(cbArg => cast[JSParameter](cbArg))
            .toList.flatMap(par => VarRes.findVarUsages(par, par.getName))
            .flatMap(usage => Option(usage.getParent)
              .flatMap(cast[JSCallExpression](_))
              .filter(call => usage eq call.getMethodExpression))
            .flatMap(call => call.getArguments.lift(0))
            .flatMap(value => ctx.subCtxEmpty().findExprType(value))
            .map(valuet => Mt.wrapPromise(valuet))
          Mt.mergeTypes(types)
        } else {
          None
        }
      case call: JSCallExpression => FuncCallRes(ctx).resolve(call)
      case vari: JSReferenceExpression => VarRes(ctx).resolve(vari)
      case indx: JSIndexedPropertyAccessExpression =>
        Option(indx.getQualifier)
          .flatMap(qual => ctx.findExprType(qual))
          .flatMap(arrT => {
            val keyTOpt = Option(indx.getIndexExpression)
              .flatMap(qua => ctx.findExprType(qua))
            Mt.getKey(arrT, keyTOpt)
          })
      case func: JSFunctionExpression =>
        val returns = getReturns(func)
        Some(new JSDeepFunctionTypeImpl(func, ctx.func(), callCtx =>
          Mt.mergeTypes(returns.flatMap(r => callCtx.findExprType(r)))
        ))
      case arr: JSArrayLiteralExpression =>
        val typeTuple = arr.getExpressions
          .map(el => ctx.findExprType(el)
            .getOrElse(JSUnknownType.JS_INSTANCE))
          .toList.asJava
        Some(new JSTupleTypeImpl(JSTypeSource.EMPTY, typeTuple, true, -1))
      case obje: JSObjectLiteralExpression =>
        val props: util.List[TypeMember] = obje.getProperties.map(p => {
          val valT = Option(p.getValue)
            .flatMap(expr => ctx.findExprType(expr))
            .getOrElse(JSUnknownType.JS_INSTANCE)
          new PropertySignatureImpl(p.getName, valT, false, new EmptyMemberSource)
        }).map(_.asInstanceOf[TypeMember]).toList.asJava
        Some(new JSRecordTypeImpl(JSTypeSource.EMPTY, props))
      case bina: JSBinaryExpression =>
        val types = List(bina.getLOperand, bina.getROperand)
          .flatMap(op => ctx.findExprType(op))
        Mt.mergeTypes(types)
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
        Mt.mergeTypes(List(tern.getThen, tern.getElse)
          .flatMap(expr => ctx.findExprType(expr)))
      case par: JSParenthesizedExpression =>
        ctx.findExprType(par.getInnerExpression)
      case pref: JSPrefixExpression =>
        if ("JS:AWAIT_KEYWORD" equals (pref.getOperationSign + "")) {
          Option(pref.getExpression)
            .flatMap(expr => ctx.findExprType(expr))
            .flatMap(pomiset => Mt.unwrapPromise(pomiset))
        } else {
          None
        }
      case _ => None
    }
  }
}
