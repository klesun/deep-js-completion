package org.klesun.deep_js_completion.structures

import java.util.Objects

import com.intellij.lang.javascript.psi.JSType
import com.intellij.lang.javascript.psi.types.{JSTypeBaseImpl, JSTypeSource}
import com.intellij.util
import com.intellij.util.ProcessingContext
import org.klesun.lang.DeepJsLang._

object EInstType extends Enumeration {
	type T = Value
	val Required, Called, New = Value
}

/**
 * a type that includes the name of npm module - needed to add
 * hardcoded logic for stuff like express which does not have a d.ts
 */
case class JSDeepModuleTypeImpl(
	val name: String, val instType: EInstType.T
) extends JSTypeBaseImpl(JSTypeSource.EMPTY) {

	override def copyTypeHierarchy(function: util.Function[_ >: JSType, _ <: JSType]): JSType = this

	override def copyWithNewSource(jsTypeSource: JSTypeSource): JSType = this

	def isEquivalentToWithSameClass(jsType: JSType, processingContext: ProcessingContext, b: Boolean): Boolean = {
		cast[JSDeepModuleTypeImpl](jsType).exists(that => that.name == name)
	}

	def isEquivalentToImpl(jsType: JSType, processingContext: ProcessingContext, b: Boolean): Boolean = isEquivalentToWithSameClass(jsType, processingContext, b)

	override def hashCodeImpl(): Int = {
		Objects.hash(List(name))
	}

	override def getTypeText(typeTextFormat: JSType.TypeTextFormat): String = {
		var typeStr = "require('" + name + "')"
		if (instType == EInstType.Called) {
			typeStr += "()"
		} else if (instType == EInstType.New) {
			typeStr = "new " + typeStr;
		}
		typeStr
	}
}
