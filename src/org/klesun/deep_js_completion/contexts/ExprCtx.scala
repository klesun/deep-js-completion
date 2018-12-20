package org.klesun.deep_js_completion.contexts

import com.intellij.lang.javascript.psi.{JSCallExpression, JSExpression, JSFunction, JSType}

case class ExprCtx(
  funcCtx: FuncCtx,
  expr: JSExpression,
  depth: Integer,
  parent: Option[ExprCtx] = None
) extends IExprCtx {

  def subExpr(expr: JSExpression, funcCtx: FuncCtx): ExprCtx = {
      new ExprCtx(funcCtx, expr, depth + 1, Some(this))
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

  override def findExprType(expr: JSExpression): Option[JSType] = {
    funcCtx.getSearch.findExprType(expr, subExpr(expr, funcCtx))
  }

  override def func(): IFuncCtx = funcCtx
}