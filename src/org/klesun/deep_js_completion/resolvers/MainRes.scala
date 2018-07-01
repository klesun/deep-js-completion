package org.klesun.deep_js_completion.resolvers

import java.util

import com.intellij.lang.javascript.psi.JSRecordType.TypeMember
import com.intellij.lang.javascript.psi._
import com.intellij.lang.javascript.psi.impl.JSLiteralExpressionImpl
import com.intellij.lang.javascript.psi.resolve.JSTypeEvaluator
import com.intellij.lang.javascript.psi.types.JSRecordMemberSourceFactory.EmptyMemberSource
import com.intellij.lang.javascript.psi.types.JSRecordTypeImpl.PropertySignatureImpl
import com.intellij.lang.javascript.psi.types._
import com.intellij.psi.PsiElement
import org.klesun.deep_js_completion.helpers.{ICtx, MultiType}
import org.klesun.lang.Lang._

import scala.collection.JavaConverters._
import scala.util.Try

object MainRes {

  private def getReturns(func: PsiElement): List[JSExpression] = {
    val arrow = cast[JSFunctionExpression](func)
      .flatMap(f => Option(f.getLastChild))
      .flatMap(cast[JSExpression](_))
    val classic = func.getChildren.toList
      .filter(c => !c.isInstanceOf[JSFunction])
      .flatMap(c => getReturns(c) ++ cast[JSReturnStatement](c)
        .flatMap(ret => Option(ret.getExpression)))
    arrow.++(classic).toList
  }

  private def getWsType(expr: JSExpression) = {
    // TODO: seems that it should be called differently . getExpressionType() looses array element/object key types
    Option(JSTypeEvaluator.getExpressionType(expr).getType)
  }

  def resolveIn(expr: JSExpression, ctx: ICtx): Option[JSType] = {

    val resolved = {expr match {
      case call: JSCallExpression => FuncCallRes(ctx).resolve(call)
      case vari: JSReferenceExpression => VarRes(ctx).resolve(vari)
      case indx: JSIndexedPropertyAccessExpression =>
        Option(indx.getQualifier)
          .flatMap(qual => ctx.findExprType(qual))
          .flatMap(arrT => {
            val keyTOpt = Option(indx.getIndexExpression)
              .flatMap(qua => ctx.findExprType(qua))
            MultiType.getKey(arrT, keyTOpt)
          })
      case func: JSFunctionExpression =>
        val types = getReturns(func)
          .flatMap(valu => ctx.findExprType(valu))
          .map(retT => new JSFunctionTypeImpl(JSTypeSource.EMPTY, new util.ArrayList[JSParameterTypeDecorator](), retT))
        MultiType.mergeTypes(types)
      case arr: JSArrayLiteralExpression =>
        val typeTuple = arr.getExpressions
          .map(el => ctx.findExprType(el)
            .getOrElse(JSUnknownType.INSTANCE))
          .toList.asJava
        Some(new JSTupleTypeImpl(JSTypeSource.EMPTY, typeTuple, true))
      case obje: JSObjectLiteralExpression =>
        val props: util.List[TypeMember] = obje.getProperties.map(p => {
          val valT = ctx.findExprType(p.getValue).getOrElse(JSUnknownType.INSTANCE)
          new PropertySignatureImpl(p.getName, valT, false, new EmptyMemberSource)
        }).map(_.asInstanceOf[TypeMember]).toList.asJava
        Some(new JSRecordTypeImpl(JSTypeSource.EMPTY, props))
      case bina: JSBinaryExpression =>
        val types = List(bina.getLOperand, bina.getROperand)
          .flatMap(op => ctx.findExprType(op))
        MultiType.mergeTypes(types)
      case lit: JSLiteralExpressionImpl =>
        if (lit.isBooleanLiteral) {
          Some(new JSBooleanLiteralTypeImpl(lit.getValue.asInstanceOf[Boolean], false, JSTypeSource.EMPTY))
        } else if (lit.isNumericLiteral) {
          Try((lit.getValue + "").toDouble).toOption
            .map(valu => new JSNumberLiteralTypeImpl(valu, false, JSTypeSource.EMPTY, lit.getValue + ""))
        } else {
          None
        }
      case _ => None
    }}: Option[JSType]

    /** @debug */
    //println("resolution of " + singleLine(expr.getText, 100) + " " + expr.getClass + " - " + resolved)

    val result = resolved.orElse(getWsType(expr))

    if (resolved.isEmpty) {
      /** @debug */
      //println("built-in of " + result)
    }

    result
  }
}
