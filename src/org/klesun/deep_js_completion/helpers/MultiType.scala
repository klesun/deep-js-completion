package org.klesun.deep_js_completion.helpers

import com.intellij.lang.javascript.psi.JSType
import com.intellij.lang.javascript.psi.types._
import com.intellij.lang.javascript.psi.types.primitives.JSUndefinedType

import scala.collection.JavaConverters._
import org.klesun.lang.Lang._

import scala.util.Try

/**
 * provides handy functions to work with JSType that can
 * either be some particular type or array of types
 */
object MultiType {
  def mergeTypes(types: List[JSType]): Option[JSType] = {
    if (types.size > 1) {
      val mt = new JSContextualUnionTypeImpl(JSTypeSource.EMPTY, types.asJava)
      Some(mt)
    } else {
      types.lift(0)
    }
  }

  def flattenTypes(t: JSType): List[JSType] = {
    t match {
      case mt: JSContextualUnionTypeImpl => {
        mt.getTypes.asScala.flatMap(mt => flattenTypes(mt)).toList
      }
      case _ => List(t)
    }
  }

  def getAllLiteralValues(litT: JSType): Option[List[String]] = {
    val opts = flattenTypes(litT).map(lit => lit match {
      case lit: JSPrimitiveLiteralType[Any] => Some(lit.getLiteral + "")
      case _ => None
    })
    all(opts)
  }

  def getKey(arrT: JSType, keyTOpt: Option[JSType]): Option[JSType] = {
    val litValsOpt = keyTOpt.flatMap(keyT => getAllLiteralValues(keyT))
    arrT match {
      case tupT: JSTupleTypeImpl =>
        val arrFallback = mergeTypes(tupT.getTypes.asScala.toList)
        val tupResultOpt = litValsOpt.map(litVals => {
          val types = litVals
            .flatMap(litVal => Try(litVal.toDouble.toInt).toOption)
            .flatMap(num => Option(tupT.getTypeByIndex(num)))
          mergeTypes(types).getOrElse(new JSUndefinedType(JSTypeSource.EMPTY))
        })
        tupResultOpt.orElse(arrFallback)
      case mt: JSContextualUnionTypeImpl =>
        val keyTypes = mt.getTypes.asScala
          .flatMap(arrT => getKey(arrT, keyTOpt)).toList
        mergeTypes(keyTypes)
      case arrT: JSArrayTypeImpl => Option(arrT.getType)
      case _ => None
    }
  }
}
