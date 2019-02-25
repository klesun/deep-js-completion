package org.klesun.deep_js_completion.contexts

import com.intellij.lang.javascript.psi.{JSCallExpression, JSExpression, JSFunction, JSType}
import com.intellij.psi.PsiElement

import scala.collection.GenTraversableOnce

case class ExprCtx(
  funcCtx: FuncCtx,
  expr: PsiElement,
  depth: Integer,
  parent: Option[ExprCtx] = None
) extends IExprCtx {

  def subExpr(expr: PsiElement, funcCtx: FuncCtx): ExprCtx = {
      ExprCtx(funcCtx, expr, depth + 1, Some(this))
  }

  def subCtxDirect(funcCall: JSCallExpression): ExprCtx = {
      subExpr(expr, funcCtx.subCtxDirect(funcCall, findExprType))
  }

  def subCtxEmpty(): ExprCtx = {
      subExpr(expr, funcCtx.subCtxEmpty())
  }

  def withClosure(funcPsi: JSFunction, closureCtx: IFuncCtx): IExprCtx = {
    subExpr(expr, funcCtx.withClosure(funcPsi, closureCtx))
  }

  override def findExprType(expr: JSExpression): GenTraversableOnce[JSType] = {
    if (expr == null) {
      None
    } else {
      funcCtx.getSearch.findExprType(expr, subExpr(expr, funcCtx))
    }
  }

  override def toString(): String = {
    "ctx: " + expr.getText
  }

  override def func(): IFuncCtx = funcCtx

  override def getDepth(): Int = depth
}
