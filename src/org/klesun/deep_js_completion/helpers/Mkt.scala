package org.klesun.deep_js_completion.helpers

import com.intellij.lang.javascript.psi.{JSType, JSTypeUtils}
import com.intellij.lang.javascript.psi.types._
import com.intellij.lang.javascript.psi.types.primitives.JSNullType
import com.intellij.psi.PsiElement
import org.klesun.deep_js_completion.structures.JSDeepMultiType

import scala.collection.GenTraversableOnce
import scala.collection.JavaConverters._
import scala.collection.immutable.List
import org.klesun.lang.DeepJsLang._

/** stands for MaKe Type - a helper with handy functions to describe typedefs */
object Mkt {
  def str(values: String*): GenTraversableOnce[JSStringLiteralTypeImpl] = {
    values.map(value => new JSStringLiteralTypeImpl(value, false, JSTypeSource.EMPTY))
  }

  def bool(value: Boolean): Some[JSBooleanLiteralTypeImpl] = {
    Some(new JSBooleanLiteralTypeImpl(value, false, JSTypeSource.EMPTY))
  }

  def bool(): Some[JSBooleanLiteralTypeImpl] = {
    bool(false)
  }

  def nul(): Some[JSNullType] = {
    Some(new JSNullType(JSTypeSource.EMPTY))
  }

  def num(value: Double): Some[JSNumberLiteralTypeImpl] = {
    Some(new JSNumberLiteralTypeImpl(value, false, JSTypeSource.EMPTY, null))
  }

  def func(): Some[JSFunctionTypeImpl] = {
    Some(new JSFunctionTypeImpl(JSTypeSource.EMPTY, List().asJava, null))
  }

  def any(): Some[JSUnknownType] = {
    Some(JSUnknownType.JS_INSTANCE)
  }

  def regexp(): Option[JSType] = {
    Option(JSTypeUtils.createType("Regexp", JSTypeSource.EMPTY))
  }

  def assoc(keys: GenTraversableOnce[(String, () => GenTraversableOnce[JSType])], psi: Option[PsiElement] = None): Some[JSRecordTypeImpl] = {
    Some(new JSRecordTypeImpl(JSTypeSource.EMPTY, keys.toList.map(t => {
      val (name, getValt) = t
      Mt.mkProp(name, getValt(), psi)
    }).asJava))
  }

  def arr(elts: GenTraversableOnce[JSType]): Some[JSArrayTypeImpl] = {
    val elt = JSDeepMultiType(elts.mem())
    Some(new JSArrayTypeImpl(elt, JSTypeSource.EMPTY))
  }

  def inst(clsName: String, generics: GenTraversableOnce[JSType] = List()): Some[JSType] = {
    Some(new JSGenericTypeImpl(JSTypeSource.EMPTY, JSTypeUtils.createType(clsName, JSTypeSource.EMPTY), generics.toList.asJava))
  }
}
