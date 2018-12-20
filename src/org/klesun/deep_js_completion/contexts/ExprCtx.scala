package org.klesun.deep_js_completion.contexts

import com.intellij.lang.javascript.psi.{JSExpression, JSType}

class ExprCtx(
  val funcCtx: FuncCtx,
  val expr: JSExpression,
  val depth: Integer,
  val parent: Option[ExprCtx] = None
) extends ICtx {

  def subExpr(expr: JSExpression, funcCtx: FuncCtx): ExprCtx = {
      new ExprCtx(funcCtx, expr, depth + 1, Some(this))
  }

  override def findExprType(expr: JSExpression): Option[JSType] = {
    funcCtx.getSearch.findExprType(expr, subExpr(expr, funcCtx))
  }
}