package org.klesun.deep_js_completion.resolvers

import java.util

import com.intellij.lang.javascript.psi.impl.{JSDefinitionExpressionImpl, JSReferenceExpressionImpl}
import com.intellij.lang.javascript.psi._
import com.intellij.psi.PsiElement
import com.intellij.psi.impl.source.resolve.reference.impl.providers.{FileReference, FileReferenceSet}
import org.klesun.deep_js_completion.helpers.{ICtx, MultiType}

import scala.collection.JavaConverters._
import org.klesun.lang.Lang._

/**
 * resolves variable type
 */
case class VarRes(ctx: ICtx) {
  def resolve(ref: JSReferenceExpression): Option[JSType] = {
    // TODO: manually support re-assignment, like
    // var someVar = null;
    // ... code
    // someVar = initializeSomething()
    Option(ref.resolve())
      .flatMap(psi => psi match {
        case para: JSParameter => Option(para.getDeclaringFunction)
          .flatMap(func => Option(func.getParent))
          .flatMap(cast[JSArgumentList](_))
          .flatMap(func => Option(func.getParent))
          .flatMap(cast[JSCallExpression](_))
          .flatMap(call => Option(call.getMethodExpression))
          .flatMap(cast[JSReferenceExpressionImpl](_))
          .filter(ref => List("forEach", "map", "filter", "sort", "reduce").contains(ref.getReferencedName))
          .flatMap(ref => Option(ref.getQualifier))
          .flatMap(expr => ctx.findExprType(expr))
          .flatMap(arrt => MultiType.getKey(arrt, None))
        case dest: JSVariable => Option(dest.getInitializer)
          .flatMap(expr => ctx.findExprType(expr))
        case prop: JSProperty => Option(prop.getValue)
          .flatMap(expr => ctx.findExprType(expr))
        case prop: JSDefinitionExpression => Option(prop.getExpression)
          .flatMap(expr => ctx.findExprType(expr))
        case _ =>
          println("Unsupported var declaration - " + psi.getClass)
          None
      })
  }
}
