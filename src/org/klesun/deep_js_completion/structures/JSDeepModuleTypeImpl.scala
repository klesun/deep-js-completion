package org.klesun.deep_js_completion.structures

import java.util.Objects

import com.intellij.lang.javascript.psi.types.{JSTypeBaseImpl, JSTypeSource}
import com.intellij.lang.javascript.psi.{JSExpression, JSFunction, JSType}
import com.intellij.psi.PsiElement
import com.intellij.psi.impl.FakePsiElement
import com.intellij.util
import com.intellij.util.ProcessingContext
import org.klesun.deep_js_completion.contexts._
import org.klesun.lang.Lang._

import scala.collection.GenTraversableOnce

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

  override def copyTypeHierarchy(function: util.Function[JSType, JSType]): JSType = this

  override def copyWithNewSource(jsTypeSource: JSTypeSource): JSType = this

  def isEquivalentToWithSameClass(jsType: JSType, processingContext: ProcessingContext, b: Boolean): Boolean = {
    cast[JSDeepModuleTypeImpl](jsType).exists(that => that.name == name)
  }

  def isEquivalentToImpl(jsType: JSType, processingContext: ProcessingContext, b: Boolean): Boolean = isEquivalentToWithSameClass(jsType, processingContext, b)

  override def resolvedHashCodeImpl(): Int = {
    Objects.hash(List(name))
  }

  def getTypeText(typeTextFormat: JSType.TypeTextFormat): String = {
    var typeStr = "require('" + name + "')"
    if (instType == EInstType.Called) {
      typeStr += "()"
    } else if (instType == EInstType.New) {
      typeStr = "new " + typeStr;
    }
    typeStr
  }
}