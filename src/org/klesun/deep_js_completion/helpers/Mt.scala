package org.klesun.deep_js_completion.helpers

import java.util

import com.intellij.lang.javascript.psi.JSRecordType.{IndexSignature, PropertySignature, TypeMember}
import com.intellij.lang.javascript.psi.JSType.TypeTextFormat
import com.intellij.lang.javascript.psi.ecma6.impl.TypeScriptFunctionSignatureImpl
import com.intellij.lang.javascript.psi.resolve.JSClassResolver
import com.intellij.lang.javascript.psi.types.JSRecordTypeImpl.IndexSignatureImpl
import com.intellij.lang.javascript.psi.types._
import com.intellij.lang.javascript.psi.types.primitives.JSUndefinedType
import com.intellij.lang.javascript.psi.{JSRecordType, JSType, JSTypeUtils}
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import com.intellij.psi.search.EverythingGlobalScope
import org.klesun.deep_js_completion.contexts.IExprCtx
import org.klesun.deep_js_completion.structures._
import org.klesun.lang.DeepJsLang.{It, _}

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
    val occurrences = new util.HashSet[JSType]()
    def internal(t: JSType): It[JSType] = {
      if (occurrences.contains(t)) {
        new It(None)
      } else {
        occurrences.add(t)
        val flattened: It[JSType] = t match {
          case mt: JSContextualUnionTypeImpl => {
            mt.getTypes.asScala.itr().flatMap(mt => internal(mt))
          }
          case mt: JSCompositeTypeImpl => {
            mt.getTypes.asScala.itr().flatMap(mt => internal(mt))
          }
          case mt: JSUnionOrIntersectionType =>
            mt.getTypes.asScala.itr().flatMap(mt => internal(mt))
          case mt: JSDeepMultiType =>
            mt.mit.itr().flatMap(mt => internal(mt))
          case _ => Some(t).itr()
        }
        flattened
      }
    }
    internal(t)
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

  private def getKeyFromMems(mems: GenTraversableOnce[TypeMember], litVals: Iterable[String]): GenTraversableOnce[JSType] = {
    val isNamed = (keyName: String) => !("" equals keyName) && !keyName.matches("\\d.*")
    mems.itr.flatMap {
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

  private def getRecordKey(objT: JSRecordType, litVals: Iterable[String]): GenTraversableOnce[JSType] = {
    getKeyFromMems(objT.getTypeMembers.asScala, litVals)
  }

  private def getKey(arrT: JSType, keyTIt: GenTraversableOnce[JSType], proj: Option[Project]): GenTraversableOnce[JSType] = {
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
      case typedef: JSType =>
        if (proj.nonEmpty) {
          val mems = getFlatMems(typedef, proj.get)
          getKeyFromMems(mems, litVals)
        } else {
          getRecordKey(typedef.asRecordType(), litVals)
        }
      case other => {
        //Console.println("Unsupported key qualifier type - " + other.getClass + " " + other)
        None
      }
    }
    elts
  }

  private def getFlatMems(typ: JSType, project: Project): GenTraversableOnce[TypeMember] = {
    val genMems = Mt.asGeneric(typ, project).itr
      .flatMap(mt => {
        val fqn = mt.getType.getTypeText(TypeTextFormat.CODE)
        val scope = new EverythingGlobalScope(project)
        val tsMems = JSClassResolver.getInstance().findClassesByQName(fqn, scope).asScala
          .itr.flatMap(ifc => ifc.getMembers.asScala)
          .flatMap(cast[TypeMember](_))
        // I suspect just asRecordType() would be enough
        mt.asRecordType().getTypeMembers.asScala ++ tsMems
      })
    var mems: GenTraversableOnce[TypeMember] = typ match {
      case objT: JSRecordType => objT.getTypeMembers.asScala
      case mt: JSType =>
        // when you specify class with jsdoc for example - JSTypeImpl
        mt.asRecordType().getTypeMembers.asScala
      case _ =>
        /** @debug*/
        //println("Unsupported typ " + typ.getClass + " " + typ)
        List()
    }
    mems = cnc(mems, genMems)
    mems.itr().map {
      case sig: TypeScriptFunctionSignatureImpl =>
        // it implements both PsiElement and TypeMember interfaces at same time
        Mt.mkProp(sig.getMemberName, Option(sig.getType), Some(sig))
      case rest => rest
      case rest => rest
    }
  }

  def getProps(objt: JSType, proj: Project): It[DeepIndexSignatureImpl] = {
    val mems = Mt.flattenTypes(objt).flatMap(t => getFlatMems(t, proj))
    mems.itr().map(mem => {
      var kpsi: Option[PsiElement] = None
      var keyt: JSType = JSUnknownType.JS_INSTANCE
      var valt: JSType = JSUnknownType.JS_INSTANCE
      mem match {
        case deep: DeepIndexSignatureImpl =>
          kpsi = deep.psi
          keyt = deep.keyt
          valt = deep.valt
        case idx: IndexSignatureImpl =>
          keyt = idx.getMemberParameterType
          valt = idx.getMemberType
        case prop: PropertySignature =>
          keyt = new JSStringLiteralTypeImpl(prop.getMemberName, true, JSTypeSource.EMPTY)
          valt = prop.getType
        case other =>
          //Console.println("unknwon flat mem: " + other.getClass + " " + other)
      }
      DeepIndexSignatureImpl(keyt, valt, kpsi)
    })
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
    val src = JSTypeSource.EMPTY
    Mt.flattenTypes(objt)
      .flatMap {
        case gen: JSGenericTypeImpl => Some(gen)
        // from js doc
        case plain: JSTypeImpl =>
          val gent = new JSGenericTypeImpl(src, plain, new util.ArrayList[JSType]())
          Some(gent)
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
      .flatMap(t => frs(
        cast[JSGenericTypeImpl](t)
          .filter(gene => List("Promise", "Bluebird")
            .contains(gene.getType.getTypeText(TypeTextFormat.CODE)))
          .map(gene => gene.getArguments.asScala.lift(0).getOrElse(JSUnknownType.JS_INSTANCE)),
        cast[JSTypeImpl](t)
          .filter(gene => List("Promise")
            .contains(gene.getTypeText(TypeTextFormat.CODE)))
          .map(gene => JSUnknownType.JS_INSTANCE),
        Some(t),
      ))
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

case class Mt(
  val proj: Option[Project]
) {
  def getKey(arrT: JSType, keyTIt: GenTraversableOnce[JSType]): GenTraversableOnce[JSType] = {
    Mt.getKey(arrT, keyTIt, proj)
  }
}
