package org.klesun.deep_js_completion.contexts

import com.intellij.lang.javascript.psi.{JSCallExpression, JSExpression, JSFunction, JSType}
import com.intellij.psi.PsiElement
import org.klesun.deep_js_completion.helpers.Mt
import org.klesun.lang.DeepJsLang.It

import scala.collection.GenTraversableOnce

/**
 * represents something that is passed to each expression kind
 * resolver for depth/recursion tracing and passed parameters typing
 */
abstract class IExprCtx {
	def func(): IFuncCtx

	def mt(): Mt

	def findExprType(expr: JSExpression): GenTraversableOnce[JSType]

	def limitResolveDepth(depthLimit: Int, expr: JSExpression): It[JSType]

	def subCtxDirect(funcCall: JSCallExpression): IExprCtx

	def subCtxEmpty(): IExprCtx

	def subCtxDoc(fakeFileSource: PsiElement): IExprCtx

	def withClosure(funcPsi: JSFunction, closureCtx: IFuncCtx): IExprCtx

	def getDepth(): Int
}
