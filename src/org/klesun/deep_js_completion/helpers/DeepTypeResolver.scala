package org.klesun.deep_js_completion.helpers

import java.util

import com.intellij.lang.javascript.psi.resolve.{JSResolveUtil, JSSimpleTypeProcessor, JSTypeEvaluator, JSTypeFromResolveResultProcessor}
import com.intellij.lang.javascript.psi._
import com.intellij.lang.javascript.psi.impl.JSFunctionExpressionImpl
import com.intellij.lang.javascript.psi.resolve.JSEvaluateContext.JSEvaluationPlace
import com.intellij.lang.javascript.psi.types._
import com.intellij.psi.PsiElement
import org.klesun.deep_js_completion.resolvers.{FuncCallRes, VarRes}
import org.klesun.lang.Lang._

import scala.collection.JavaConverters._

object DeepTypeResolver {

  def getReturns(func: PsiElement): List[JSExpression] = {
    func.getChildren.toList
      .filter(c => !c.isInstanceOf[JSFunction])
      .flatMap(c => getReturns(c) ++ cast[JSReturnStatement](c)
        .flatMap(ret => Option(ret.getExpression)))
  }

  def getWsType(expr: JSExpression) = {
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
      case _ => None
    }}: Option[JSType]

    /** @debug */
    //println("resolution of " + expr.getText + " " + expr.getClass + " - " + resolved)

    val result = resolved.orElse(getWsType(expr))

    if (resolved.isEmpty) {
      /** @debug */
      //println("built-in of " + result)
    }

    result
  }
}
