package org.klesun.deep_js_completion.helpers

import java.util

import com.intellij.lang.javascript.psi.JSRecordType.{IndexSignature, PropertySignature, TypeMember}
import com.intellij.lang.javascript.psi.JSType.TypeTextFormat
import com.intellij.lang.javascript.psi.types._
import com.intellij.lang.javascript.psi.types.primitives.JSUndefinedType
import com.intellij.lang.javascript.psi.{JSRecordType, JSType, JSTypeUtils}
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import org.klesun.deep_js_completion.contexts.IExprCtx
import org.klesun.deep_js_completion.structures._
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
  def mergeTypes(tit: GenTraversableOnce[JSType]): Option[JSType] = {
    val types = tit.toList
    if (types.size > 1) {
      val mt = new JSContextualUnionTypeImpl(JSTypeSource.EMPTY, types.toList.distinct.asJava)
      Some(mt)
    } else {
      types.lift(0)
    }
  }

  def flattenTypes(t: JSType): It[JSType] = {
    ({t match {
      case mt: JSContextualUnionTypeImpl => {
        mt.getTypes.asScala.flatMap(mt => flattenTypes(mt))
      }
      case mt: JSCompositeTypeImpl => {
        mt.getTypes.asScala.flatMap(mt => flattenTypes(mt))
      }
      case mt: JSUnionOrIntersectionType =>
        mt.getTypes.asScala.flatMap(mt => flattenTypes(mt))
      case mt: JSDeepMultiType =>
        mt.mit.itr()
      case _ => Some(t)
    }}: GenTraversableOnce[JSType]).itr()
  }

  private def getLiteralValueOpts(litT: JSType): Iterable[Option[String]] = {
    flattenTypes(litT).itr().map {
      case lit: JSPrimitiveLiteralType[_] => Some(lit.getLiteral + "")
      case _ => None
    }.toIterable
  }

  def getAnyLiteralValues(litT: JSType): Iterable[String] = {
    getLiteralValueOpts(litT).flatten
  }

  def getAllLiteralValues(litT: JSType): Option[Iterable[String]] = {
    val opts = getLiteralValueOpts(litT)
    all(opts)
  }

  private def getRecordKey(objT: JSRecordType, litVals: Iterable[String]): GenTraversableOnce[JSType] = {
    val isNamed = (keyName: String) => !("" equals keyName) && !keyName.matches("\\d.*")
    objT.getTypeMembers.asScala
      .flatMap {
        case mem: PropertySignature => Option(mem.getType)
          .filter(t => litVals.isEmpty || litVals.exists(lit => lit equals mem.getMemberName))
          .filter(t => {
            val isEs5PromiseThen = (mem.getMemberName equals "then") &&
              (t + "").endsWith("): Promise<TResult1|TResult2>")
            // es2015 d.ts has some weird return type - Promise<TResult1 | TResult2>,
            // it results in irrelevant options, so I'm overriding it here
            !isEs5PromiseThen
          })
        case idx: IndexSignature =>
          val propMatches = getAllLiteralValues(idx.getMemberParameterType) match {
            case Some(strvals) => !litVals.exists(isNamed) ||
              !strvals.exists(isNamed) ||
              strvals.toList.intersect(litVals.toList).nonEmpty
            case None => true
          }
          if (propMatches) Some(idx.getMemberType) else None
        case other => {
          None
        }
      }
  }

  def getKey(arrT: JSType, keyTIt: GenTraversableOnce[JSType]): GenTraversableOnce[JSType] = {
    // TODO: it seems I broke something here. result.headerData. suggests me just 'passengerList'
    // TODO: should optimize to stop right after first
    //  "unknown" key type, like it's done in deep-assoc
    val keyTOpt = Mt.mergeTypes(keyTIt)
    val litValsOpt = keyTOpt.flatMap(keyT => getAllLiteralValues(keyT))
    val litVals = litValsOpt.toIterable.flatten
    val elts = Mt.flattenTypes(arrT).flatMap {
      case tupT: JSTupleTypeImpl =>
        val arrFallback = mergeTypes(tupT.getTypes.asScala.itr)
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
    elts
  }

  def getReturnType(funcT: JSType, ctx: IExprCtx): GenTraversableOnce[JSType] = {
    val retTs = flattenTypes(funcT)
      .flatMap {
        case func: JSFunctionTypeImpl => Option(func.getReturnType)
        case func: JSDeepFunctionTypeImpl => func.getReturnType(ctx)
        case func: JSDeepModuleTypeImpl => Some(JSDeepModuleTypeImpl(func.name, EInstType.Called))
        case _ => None
      }
    retTs
  }

  def asGeneric(objt: JSType, project: Project): GenTraversableOnce[JSGenericTypeImpl] = {
    Mt.flattenTypes(objt)
      .flatMap {
        case gen: JSGenericTypeImpl => Some(gen)
        case arr: JSArrayType => Option(arr.asGenericType())
        case tupt: JSTupleTypeImpl =>
          val elt: JSType = JSDeepMultiType(tupt.getTypes.asScala.mem())
          val fqnType = JSTypeUtils.createType("Array", JSTypeSource.EMPTY)
          Some(new JSGenericTypeImpl(JSTypeSource.EMPTY, fqnType, List(elt).asJava))
        case funct: JSDeepFunctionTypeImpl =>
          val fqnType = JSTypeUtils.createType("Function", JSTypeSource.EMPTY)
          Some(new JSGenericTypeImpl(JSTypeSource.EMPTY, fqnType, new util.ArrayList[JSType]()))
        case _ => None
      }
  }

  def unwrapPromise(promiset: JSType): It[JSType] = {
    flattenTypes(promiset)
      .map(t => {
        cast[JSGenericTypeImpl](t)
          .filter(gene => List("Promise", "Bluebird")
            .contains(gene.getType.getTypeText(TypeTextFormat.CODE)))
          .map(gene => gene.getArguments.asScala.lift(0).getOrElse(JSUnknownType.JS_INSTANCE))
          .getOrElse(t)
      })
  }

  def wrapPromise(value: JSType): JSType = {
    Mkt.inst("Promise", List(value)).get
  }

  def mkProp(name: String, valtit: GenTraversableOnce[JSType], psi: Option[PsiElement] = None): TypeMember = {
    val keyt = new JSStringLiteralTypeImpl(name, true, JSTypeSource.EMPTY)
    val valt = JSDeepMultiType(valtit.mem())
    DeepIndexSignatureImpl(keyt, valt, psi)
  }
}
