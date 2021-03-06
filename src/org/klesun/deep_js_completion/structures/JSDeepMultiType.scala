package org.klesun.deep_js_completion.structures

import com.intellij.lang.javascript.psi.JSType
import com.intellij.lang.javascript.psi.types.{JSTypeBaseImpl, JSTypeSource}
import com.intellij.util
import com.intellij.util.ProcessingContext
import org.klesun.lang.DeepJsLang._

/**
 * a wrapper for a type iterator that implements JSType so
 * that it could be passed to built-in IndexSignature and such
 */
case class JSDeepMultiType(
	mit: MemIt[JSType],
) extends JSTypeBaseImpl(JSTypeSource.EMPTY) {

	override def copyTypeHierarchy(function: util.Function[_ >: JSType, _ <: JSType]): JSType = this

	override def copyWithNewSource(jsTypeSource: JSTypeSource): JSType = this

	def isEquivalentToWithSameClass(jsType: JSType, processingContext: ProcessingContext, b: Boolean): Boolean = {
		cast[JSDeepMultiType](jsType).exists(that => that.mit == mit)
	}

	def isEquivalentToImpl(jsType: JSType, processingContext: ProcessingContext, b: Boolean): Boolean = isEquivalentToWithSameClass(jsType, processingContext, b)

	override def hashCodeImpl(): Int = {
		mit.hashCode()
	}

	override def getTypeText(typeTextFormat: JSType.TypeTextFormat): String = {
		if (mit.complete) {
			mit.itr().mkString("|")
		} else {
			"*|*|*"
			// uncomment for debug
			mit.itr().toList + ""
		}
	}
}
