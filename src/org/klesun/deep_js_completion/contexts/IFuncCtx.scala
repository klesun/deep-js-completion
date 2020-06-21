package org.klesun.deep_js_completion.contexts

import com.intellij.lang.javascript.psi.types.JSArrayType
import com.intellij.lang.javascript.psi.{JSFunction, JSType}

import scala.collection.GenTraversableOnce

/**
 * implementation should override hashCode() and equals() as well,
 * since context is used as a key in expression type caching
 */
abstract class IFuncCtx {
	def getArg(order: Integer): GenTraversableOnce[JSType]

	def areArgsKnown(): Boolean

	def hasArgs(): Boolean

	def isInComment(): Boolean

	def getSpreadArg(): JSArrayType

	def getClosurePsi(): Option[JSFunction]

	def getClosureCtx(): Option[IFuncCtx]
}
