package org.klesun.deep_js_completion.contexts

import com.intellij.lang.javascript.psi.{JSCallExpression, JSExpression, JSFunction, JSType}

/**
 * represents something that is passed to each expression kind
 * resolver for depth/recursion tracing and passed parameters typing
 */
abstract class IExprCtx {
  def func(): IFuncCtx
  def findExprType(expr: JSExpression): Option[JSType]
  def subCtxDirect(funcCall: JSCallExpression): IExprCtx
  def subCtxEmpty(): IExprCtx
  def withClosure(funcPsi: JSFunction, closureCtx: IFuncCtx): IExprCtx
}
