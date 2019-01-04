package org.klesun.deep_js_completion.helpers

import com.intellij.lang.javascript.psi.JSRecordType.{IndexSignature, TypeMember}
import com.intellij.lang.javascript.psi.JSType.TypeTextFormat
import com.intellij.lang.javascript.psi.types.JSRecordTypeImpl.PropertySignatureImpl
import com.intellij.lang.javascript.psi.types._
import com.intellij.lang.javascript.psi.types.primitives.JSUndefinedType
import com.intellij.lang.javascript.psi.{JSRecordType, JSType, JSTypeUtils}
import com.intellij.psi.PsiElement
import org.klesun.deep_js_completion.contexts.IExprCtx
import org.klesun.deep_js_completion.structures.{DeepIndexSignatureImpl, JSDeepFunctionTypeImpl}
import org.klesun.lang.Lang._

import scala.collection.GenTraversableOnce
import scala.collection.JavaConverters._
import scala.collection.immutable.List
import scala.util.Try

/**
 * stands for Multi-Type
 * provides handy functions to work with JSType that can
 * either be some particular type or array of types
 */
object Mt {
  def mergeTypes(types: GenTraversableOnce[JSType]): Option[JSType] = {
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

  private def getLiteralValueOpts(litT: JSType): List[Option[String]] = {
    flattenTypes(litT).map {
      case lit: JSPrimitiveLiteralType[_] => Some(lit.getLiteral + "")
      case _ => None
    }
  }

  def getAnyLiteralValues(litT: JSType): List[String] = {
    getLiteralValueOpts(litT).flatten
  }

  def getAllLiteralValues(litT: JSType): Option[List[String]] = {
    val opts = getLiteralValueOpts(litT)
    all(opts)
  }

  def getKey(arrT: JSType, keyTOpt: Option[JSType]): Option[JSType] = {
    val litValsOpt = keyTOpt.flatMap(keyT => getAllLiteralValues(keyT))
    val litVals = litValsOpt.toList.flatten
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
          .flatMap {
            case mem: PropertySignatureImpl => Option(mem.getType)
              .filter(t => litVals.isEmpty || litVals.contains(mem.getMemberName))
            case idx: IndexSignature =>
              val propMatches = getAllLiteralValues(idx.getMemberParameterType) match {
                case Some(strvals) => strvals.isEmpty || strvals.intersect(litVals).nonEmpty
                case None => true
              }
              if (propMatches) Some(idx.getMemberType) else None
            case _ => None
          }
        mergeTypes(keyTypes)
      case _ => None
    }
  }

  def getReturnType(funcT: JSType, ctx: IExprCtx): Option[JSType] = {
    val retTs = flattenTypes(funcT)
      .flatMap {
        case func: JSFunctionTypeImpl => Option(func.getReturnType)
        case func: JSDeepFunctionTypeImpl => func.getReturnType(ctx)
        case _ => None
      }
    mergeTypes(retTs)
  }

  def unwrapPromise(promiset: JSType): Option[JSType] = {
    val promisedt = flattenTypes(promiset)
      .flatMap(cast[JSGenericTypeImpl](_))
      .filter(gene => List("Promise", "Bluebird")
        .contains(gene.getType.getTypeText(TypeTextFormat.CODE)))
      .flatMap(gene => gene.getArguments.asScala.lift(0))
    mergeTypes(promisedt)
  }

  def wrapPromise(value: JSType): JSType = {
    new JSGenericTypeImpl(JSTypeSource.EMPTY, JSTypeUtils.createType("Promise", JSTypeSource.EMPTY), List(value).asJava)
  }

  def mkProp(name: String, psi: PsiElement, getValue: () => GenTraversableOnce[JSType]): TypeMember = {
    val keyt = new JSStringLiteralTypeImpl(name, true, JSTypeSource.EMPTY)
    val valt = Mt.mergeTypes(getValue()).getOrElse(JSUnknownType.JS_INSTANCE)
    new DeepIndexSignatureImpl(keyt, valt, psi)
  }
}
