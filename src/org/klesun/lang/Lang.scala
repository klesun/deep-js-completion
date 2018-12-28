package org.klesun.lang

import com.intellij.psi.PsiElement

import scala.reflect.{ClassTag, classTag}

/** provides some core functions needed for IDEA plugin development */
object Lang {
  // it would be nice to tell compiler somehow that T
  // is subclass of value arg... I could not find the way
  def cast[T : ClassTag](value: Object): Option[T] = {
    value match {
      /** @see https://stackoverflow.com/a/21640639/2750743 */
      case matched: T if classTag[T].runtimeClass.isInstance(matched) => Some(matched)
      case _ => None
    }
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

  def all[T](opts: List[Option[T]]): Option[List[T]] = {
    if (opts.exists(o => o.isEmpty)) {
      None
    } else {
      Some(opts.map(o => o.get))
    }
  }

  def singleLine(text: String, length: Int): String = {
    val lines = text.split("\n").map(l => l.trim)
    substr(lines.mkString(" "), 0, length)
  }

  def findParent[T <: PsiElement : ClassTag](psi: PsiElement): Option[T] = {
    var parent = psi.getParent
    var matching: Option[T] = None
    while (parent != null && matching.isEmpty) {
      matching = cast[T](parent)
      parent = parent.getParent
    }
    matching
  }

  def findChildren[T <: PsiElement : ClassTag](parent: PsiElement): List[T] = {
    parent.getChildren
      .flatMap(c => findChildren[T](c))
      .++(List(parent).flatMap(cast[T](_)))
      .toList
  }

  import java.io.PrintWriter
  import java.io.StringWriter

  def getStackTrace(exc: Throwable): String = {
    val sw = new StringWriter
    val pw = new PrintWriter(sw)
    exc.printStackTrace(pw)
    sw.toString
  }

  def getStackTrace: String = getStackTrace(new Exception)
}
