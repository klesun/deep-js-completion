package org.klesun.deep_js_completion.structures

import java.util.Objects

import com.intellij.lang.javascript.psi.{JSExpression, JSFunction, JSFunctionType, JSType}
import com.intellij.lang.javascript.psi.types.{JSTypeBaseImpl, JSTypeSource}
import com.intellij.psi.PsiElement
import com.intellij.psi.impl.FakePsiElement
import com.intellij.util
import com.intellij.util.ProcessingContext
import org.klesun.deep_js_completion.contexts._

import scala.collection.GenTraversableOnce
import org.klesun.lang.Lang._

/**
  * unlike built-in JSFunctionTypeImpl, this one has the getReturnType(context)
  * function which returns the return type that depends on passed arguments
  */
case class JSDeepFunctionTypeImpl(
  val funcPsi: JSFunction, // to distinct what args belong to _this context_ during resolution
  val closureCtx: IFuncCtx,
  val returnTypeGetter: IExprCtx => GenTraversableOnce[JSType],
) extends JSTypeBaseImpl(JSTypeSource.EMPTY) {

  override def copyTypeHierarchy(function: util.Function[JSType, JSType]): JSType = this

  override def copyWithNewSource(jsTypeSource: JSTypeSource): JSType = this

  def isEquivalentToWithSameClass(jsType: JSType, processingContext: ProcessingContext, b: Boolean): Boolean = {
    cast[JSDeepFunctionTypeImpl](jsType).exists(that => that.funcPsi equals this.funcPsi)
  }

  def isEquivalentToImpl(jsType: JSType, processingContext: ProcessingContext, b: Boolean): Boolean = isEquivalentToWithSameClass(jsType, processingContext, b)

  override def resolvedHashCodeImpl(): Int = {
    Objects.hash(List(funcPsi))
  }

  def getTypeText(typeTextFormat: JSType.TypeTextFormat): String = "Function"

//  override def toString(): String = {
//    val funcCtx = FuncCtx(new SearchCtx())
//    val exprCtx = ExprCtx(funcCtx, new FakePsiElement with JSExpression {
//      override def getParent: PsiElement = null
//      override def replace(jsExpression: JSExpression): JSExpression = ???
//    }, 0)
//    "() => " + getReturnType(exprCtx)
//  }

  def getReturnType(ctx: IExprCtx): GenTraversableOnce[JSType] = {
    returnTypeGetter(ctx.withClosure(funcPsi, closureCtx))
  }
}
