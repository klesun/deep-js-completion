package org.klesun.deep_js_completion.helpers

import com.intellij.lang.javascript.psi.JSType.TypeTextFormat
import com.intellij.lang.javascript.psi.types.JSRecordTypeImpl.PropertySignatureImpl
import com.intellij.lang.javascript.psi.{JSFunctionExpression, JSRecordType, JSType}
import com.intellij.lang.javascript.psi.types._
import com.intellij.lang.javascript.psi.types.primitives.JSUndefinedType

import scala.collection.JavaConverters._
import org.klesun.lang.Lang._

import scala.collection.GenTraversableOnce
import scala.util.Try

/**
 * stands for Multi-Type
 * provides handy functions to work with JSType that can
 * either be some particular type or array of types
 */
object Mt {
  def mergeTypes(types: Iterable[JSType]): Option[JSType] = {
    if (types.size > 1) {
      val mt = new JSContextualUnionTypeImpl(JSTypeSource.EMPTY, types.toList.asJava)
      Some(mt)
    } else {
      types.toList.lift(0)
    }
  }

  def flattenTypes(t: JSType): List[JSType] = {
    t match {
      case mt: JSContextualUnionTypeImpl => {
        mt.getTypes.asScala.flatMap(mt => flattenTypes(mt)).toList
      }
      case mt: JSCompositeTypeImpl => {
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
      case objT: JSRecordType =>
        val keyTypes = objT.getTypeMembers.asScala
          .flatMap(cast[PropertySignatureImpl](_))
          .filter(mem => litValsOpt.map(vals => vals
            .contains(mem.getMemberName)).getOrElse(true))
          .flatMap(mem => Option(mem.getType))
        mergeTypes(keyTypes)
      case _ => None
    }
  }

  def getReturnType(funcT: JSType): Option[JSType] = {
    val retTs = flattenTypes(funcT)
      .flatMap(cast[JSFunctionTypeImpl](_))
      .flatMap(func => Option(func.getReturnType))
    mergeTypes(retTs)
  }

  def getPromiseValue(promiset: JSType): Option[JSType] = {
    val promisedt = flattenTypes(promiset)
      .flatMap(cast[JSGenericTypeImpl](_))
      .filter(gene => List("Promise", "Bluebird")
        .contains(gene.getType.getTypeText(TypeTextFormat.CODE)))
      .flatMap(gene => gene.getArguments.asScala.lift(0))
    mergeTypes(promisedt)
  }
}
