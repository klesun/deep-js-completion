package org.klesun.lang

import com.intellij.psi.PsiElement

import scala.reflect.{ClassTag, classTag}
import scala.reflect.runtime.universe._

/** provides some core functions needed for IDEA plugin development */
object Lang {
  def cast[T : ClassTag](value: Object): Option[T] = {
    //println("checking val type " + value.getClass + " against expected " + classTag[T])
    value match {
      /** @see https://stackoverflow.com/a/21640639/2750743 */
      case matched: T if classTag[T].runtimeClass.isInstance(matched) => Some(matched)
      case _ => None
    }
//    if (value.isInstanceOf[T]) {
//      Option(value.asInstanceOf[T])
//    } else {
//      None
//    }
  }

  // looks like it's to complex for Scala compiler so I guess I won't use it after all...
//  def toCast[B, A <: PsiElement]: A => Option[B] = {
//    obj => cast[B](obj)
//  }

  /**
   * you can use it to println() some text to console without breaking Option chain:
   * Option(...)
   *   .map(...)
   *   .filter(val => Lang.log("some text " + val))
   *   .map(...)
   */
  def log(msg: String) = {
    println(msg)
    true
  }

  def substr(str: String, startIndexArg: Int, endIndexArg: Int): String = {
    var startIndex = startIndexArg
    var endIndex = endIndexArg
    if (startIndex < 0) {
        startIndex = str.length() + startIndex
    }
    if (endIndex < 0) {
        endIndex = str.length() + endIndex
    }

    if (str.length() > 0 && startIndex < endIndex) {
      str.substring(
          Math.max(0, startIndex),
          Math.min(str.length(), endIndex)
      )
    } else {
      ""
    }
  }

  def substr(str: String, startIndex: Int): String = {
      substr(str, startIndex, str.length())
  }
}
