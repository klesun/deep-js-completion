package org.klesun.deep_js_completion.contexts

import com.intellij.lang.javascript.psi.{JSCallExpression, JSExpression, JSFunction, JSType}
import com.intellij.psi.PsiElement
import org.klesun.deep_js_completion.helpers
import org.klesun.deep_js_completion.helpers.Mt
import org.klesun.lang.DeepJsLang.It

import scala.collection.GenTraversableOnce
import org.klesun.lang.DeepJsLang._

case class ExprCtx(
	funcCtx: FuncCtx,
	expr: PsiElement,
	depth: Integer,
	parent: Option[ExprCtx] = None,
	doNotCache: Boolean = false,
) extends IExprCtx {

	def subExpr(expr: PsiElement, funcCtx: FuncCtx): ExprCtx = {
		val sub = ExprCtx(funcCtx, expr, depth + 1, Some(this), doNotCache = this.doNotCache)
		sub
	}

	def subCtxDirect(funcCall: JSCallExpression): ExprCtx = {
		subExpr(expr, funcCtx.subCtxDirect(funcCall, findExprType))
	}

	def subCtxEmpty(): ExprCtx = {
		subExpr(expr, funcCtx.subCtxEmpty())
	}

	def subCtxDoc(fakeFileSource: PsiElement): ExprCtx = {
		val funcSubCtx = FuncCtx(
			search = funcCtx.search,
			parent = Some(funcCtx),
			fakeFileSource = Some(fakeFileSource)
		)
		subExpr(expr, funcSubCtx)
	}

	def withClosure(funcPsi: JSFunction, closureCtx: IFuncCtx): IExprCtx = {
		subExpr(expr, funcCtx.withClosure(funcPsi, closureCtx))
	}

	override def findExprType(expr: JSExpression): GenTraversableOnce[JSType] = {
		if (expr == null) {
			None
		} else {
			// the flatMap trick creates an iterator that will actually resolve
			// this expression only when you start iterating through the types - irreplaceable
			// in object resolution where you will effectively need just one field
			nit(true).flatMap(b => {
				funcCtx.getSearch.findExprType(expr, subExpr(expr, funcCtx))
			})
		}
	}

	override def limitResolveDepth(depthLimit: Int, expr: JSExpression): It[JSType] = {
		val depth = Math.max(funcCtx.search.maxDepth - depthLimit, this.depth)
		val nextCtx = ExprCtx(funcCtx, expr, depth, Some(this), doNotCache = true)
		nextCtx.findExprType(expr).itr()
	}

	override def toString(): String = {
		"ctx: " + expr.getText
	}

	override def func(): IFuncCtx = funcCtx

	override def mt(): Mt = helpers.Mt(funcCtx.search.project)

	override def getDepth(): Int = depth
}
