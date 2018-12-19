package org.klesun.deep_js_completion.contexts

import com.intellij.lang.javascript.psi.{JSExpression, JSType}

/**
 * represents something that is passed to each expression kind
 * resolver for depth/recursion tracing and passed parameters typing
 */
abstract class ICtx {
  def findExprType(expr: JSExpression): Option[JSType]
}
