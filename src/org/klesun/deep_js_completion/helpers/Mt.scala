package org.klesun.deep_js_completion.helpers

import java.util

import com.intellij.lang.javascript.psi.JSRecordType.{IndexSignature, PropertySignature, TypeMember}
import com.intellij.lang.javascript.psi.JSType.TypeTextFormat
import com.intellij.lang.javascript.psi.ecma6.impl.TypeScriptInterfaceImpl
import com.intellij.lang.javascript.psi.ecmal4.JSClass
import com.intellij.lang.javascript.psi.resolve.{JSClassResolver, JSScopeNamesCache, JSTypeEvaluator}
import com.intellij.lang.javascript.psi.types.JSRecordTypeImpl.PropertySignatureImpl
import com.intellij.lang.javascript.psi.types._
import com.intellij.lang.javascript.psi.types.primitives.JSUndefinedType
import com.intellij.lang.javascript.psi.{JSRecordType, JSType, JSTypeUtils, JSVariable}
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import com.intellij.psi.search.EverythingGlobalScope
import com.intellij.psi.stubs.StubElement
import org.klesun.deep_js_completion.completion_providers.PropNamePvdr.getMems
import org.klesun.deep_js_completion.contexts.IExprCtx
import org.klesun.deep_js_completion.resolvers.VarRes
import org.klesun.deep_js_completion.structures._
import org.klesun.lang.Lang
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
      val mt = new JSContextualUnionTypeImpl(JSTypeSource.EMPTY, types.toList.distinct.asJava)
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
      case mt: JSUnionOrIntersectionType =>
        mt.getTypes.asScala.flatMap(mt => flattenTypes(mt)).toList
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

  private def getRecordKey(objT: JSRecordType, litVals: List[String]): GenTraversableOnce[JSType] = {
    val isNamed = (keyName: String) => !("" equals keyName) && !keyName.matches("\\d.*")
    objT.getTypeMembers.asScala
      .flatMap {
        case mem: PropertySignature => Option(mem.getType)
          .filter(t => litVals.isEmpty || litVals.contains(mem.getMemberName))
          .filter(t => {
            val isEs5PromiseThen = (mem.getMemberName equals "then") &&
              (t + "").endsWith("): Promise<TResult1|TResult2>")
            // es2015 d.ts has some weird return type - Promise<TResult1 | TResult2>,
            // it results in irrelevant options, so I'm overriding it here
            !isEs5PromiseThen
          })
        case idx: IndexSignature =>
          val propMatches = getAllLiteralValues(idx.getMemberParameterType) match {
            case Some(strvals) => !litVals.exists(isNamed) || !strvals.exists(isNamed) || strvals.intersect(litVals).nonEmpty
            case None => true
          }
          if (propMatches) Some(idx.getMemberType) else None
        case other => {
          None
        }
      }
  }

  def getKey(arrT: JSType, keyTOpt: Option[JSType]): Option[JSType] = {
    val litValsOpt = keyTOpt.flatMap(keyT => getAllLiteralValues(keyT))
    val litVals = litValsOpt.toList.flatten
    val elts = Mt.flattenTypes(arrT).flatMap {
      case tupT: JSTupleTypeImpl =>
        val arrFallback = mergeTypes(tupT.getTypes.asScala.toList)
        val tupResultOpt = litValsOpt.map(litVals => {
          val types = litVals
            .flatMap(litVal => Try(litVal.toDouble.toInt).toOption)
            .flatMap(num => Option(tupT.getTypeByIndex(num)))
          mergeTypes(types).getOrElse(new JSUndefinedType(JSTypeSource.EMPTY))
        })
        tupResultOpt.orElse(arrFallback)
      case arrT: JSArrayTypeImpl => Option(arrT.getType)
      case objT: JSRecordType => getRecordKey(objT, litVals)
      case typedef: JSType => getRecordKey(typedef.asRecordType(), litVals)
      case other => {
        //Console.println("Unsupported key qualifier type - " + other.getClass + " " + other)
        None
      }
    }
    Mt.mergeTypes(elts)
  }

  def getReturnType(funcT: JSType, ctx: IExprCtx): Option[JSType] = {
    val retTs = flattenTypes(funcT)
      .flatMap {
        case func: JSFunctionTypeImpl => Option(func.getReturnType)
        case func: JSDeepFunctionTypeImpl => func.getReturnType(ctx)
        case func: JSDeepModuleTypeImpl => Some(JSDeepModuleTypeImpl(func.name, EInstType.Called))
        case _ => None
      }
    mergeTypes(retTs)
  }

  def asGeneric(objt: JSType, project: Project): GenTraversableOnce[JSGenericTypeImpl] = {
    Mt.flattenTypes(objt)
      .flatMap {
        case gen: JSGenericTypeImpl => Some(gen)
        case arr: JSArrayType => Option(arr.asGenericType())
        case tupt: JSTupleTypeImpl =>
          val elt = Mt.mergeTypes(tupt.getTypes.asScala).getOrElse(JSUnknownType.JS_INSTANCE)
          val fqnType = JSTypeUtils.createType("Array", JSTypeSource.EMPTY)
          Some(new JSGenericTypeImpl(JSTypeSource.EMPTY, fqnType, List(elt).asJava))
        case funct: JSDeepFunctionTypeImpl =>
          val fqnType = JSTypeUtils.createType("Function", JSTypeSource.EMPTY)
          Some(new JSGenericTypeImpl(JSTypeSource.EMPTY, fqnType, new util.ArrayList[JSType]()))
        case _ => None
      }
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
    Mkt.inst("Promise", List(value)).get
  }

  def mkProp(name: String, getValue: () => GenTraversableOnce[JSType], psi: Option[PsiElement] = None): TypeMember = {
    val keyt = new JSStringLiteralTypeImpl(name, true, JSTypeSource.EMPTY)
    val valt = Mt.mergeTypes(getValue()).getOrElse(JSUnknownType.JS_INSTANCE)
    DeepIndexSignatureImpl(keyt, valt, psi)
  }
}
