package org.klesun.deep_js_completion.structures

import java.util.Objects

import com.intellij.lang.javascript.psi.types.{JSTypeBaseImpl, JSTypeSource}
import com.intellij.lang.javascript.psi.{JSFunction, JSType}
import com.intellij.util
import com.intellij.util.ProcessingContext
import org.klesun.deep_js_completion.contexts._
import org.klesun.lang.DeepJsLang._

import scala.collection.GenTraversableOnce

/**
 * unlike built-in JSFunctionTypeImpl, this one has the getReturnType(context)
 * function which returns the return type that depends on passed arguments
 */
case class JSDeepFunctionTypeImpl(
	val funcPsi: JSFunction, // to distinct what args belong to _this context_ during resolution
	val returnTypeGetter: IExprCtx => GenTraversableOnce[JSType],
	val closureCtx: Option[IFuncCtx] = None,
) extends JSTypeBaseImpl(JSTypeSource.EMPTY) {

	override def copyTypeHierarchy(function: util.Function[_ >: JSType, _ <: JSType]): JSType = this

	override def copyWithNewSource(jsTypeSource: JSTypeSource): JSType = this

	def isEquivalentToWithSameClass(jsType: JSType, processingContext: ProcessingContext, b: Boolean): Boolean = {
		cast[JSDeepFunctionTypeImpl](jsType).exists(that => that.funcPsi equals this.funcPsi)
	}

	def isEquivalentToImpl(jsType: JSType, processingContext: ProcessingContext, b: Boolean): Boolean = isEquivalentToWithSameClass(jsType, processingContext, b)

	override def hashCodeImpl(): Int = {
		Objects.hash(List(funcPsi))
	}

	override def getTypeText(typeTextFormat: JSType.TypeTextFormat): String = "Function"

	//  override def toString(): String = {
	//    val funcCtx = FuncCtx(new SearchCtx(project=Option(funcPsi.getProject)))
	//    val exprCtx = ExprCtx(funcCtx, new FakePsiElement with JSExpression {
	//      override def getParent: PsiElement = null
	//      override def replace(jsExpression: JSExpression): JSExpression = ???
	//    }, 0)
	//    "() => " + getReturnType(exprCtx)
	//  }

	def getReturnType(ctx: IExprCtx): GenTraversableOnce[JSType] = {
		val finalCtx = closureCtx.map(closureCtx => ctx.withClosure(funcPsi, closureCtx))
			.getOrElse(ctx)
		returnTypeGetter(finalCtx)
	}
}
