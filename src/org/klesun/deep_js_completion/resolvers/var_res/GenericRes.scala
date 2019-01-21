package org.klesun.deep_js_completion.resolvers.var_res

import com.intellij.lang.javascript.psi.{JSType, JSTypeUtils}
import com.intellij.lang.javascript.psi.ecma6._
import com.intellij.lang.javascript.psi.types._
import org.klesun.deep_js_completion.contexts.IExprCtx
import org.klesun.deep_js_completion.helpers.Mt
import org.klesun.deep_js_completion.resolvers.var_res.GenericRes.{getGenericTypeFromArg, parseTypePsi, resolveTypeExpr}
import org.klesun.deep_js_completion.structures.JSDeepFunctionTypeImpl
import org.klesun.lang.Lang.cast

import scala.collection.{GenTraversableOnce, mutable}
import scala.collection.JavaConverters._
import scala.collection.mutable


object GenericRes {
  /** get the type of typePsi replacing generics with actual values */
  private def parseTypePsi(typePsi: JSTypeDeclaration, generics: Map[String, () => Array[JSType]]): GenTraversableOnce[JSType] = {
    typePsi match {
      case arrts: TypeScriptArrayType =>
        val elts = Option(arrts.getType).toList
          .flatMap(eltPsi => GenericRes.parseTypePsi(eltPsi, generics))
        val arrt = new JSArrayTypeImpl(Mt.mergeTypes(elts).orNull, JSTypeSource.EMPTY)
        Some(arrt)
      case arrts: TypeScriptTupleType =>
        val els = arrts.getElements
          .map(eltPsi => GenericRes.parseTypePsi(eltPsi, generics))
          .map(oneElTypes => Mt.mergeTypes(oneElTypes).getOrElse(JSUnknownType.JS_INSTANCE))

        val tupt = new JSTupleTypeImpl(JSTypeSource.EMPTY, els.toList.asJava, true, -1)
        Some(tupt)
      case functs: TypeScriptFunctionType =>
        val rettypes = parseTypePsi(functs.getReturnTypeElement, generics)
        val rett = Mt.mergeTypes(rettypes).getOrElse(JSUnknownType.JS_INSTANCE)
        val argtypes = functs.getParameters.map(arg => {
          val argtypes = Option(arg.getTypeElement)
            .flatMap(cast[TypeScriptType](_)).toList
            .flatMap(argts => parseTypePsi(argts, generics))
          val argt = Mt.mergeTypes(argtypes).getOrElse(JSUnknownType.JS_INSTANCE)
          new JSParameterTypeDecoratorImpl(argt, arg.isOptional, arg.isRest, true)
        })
        val funct = new JSFunctionTypeImpl(JSTypeSource.EMPTY, argtypes.toList.asJava, rett)
        Some(funct)
      case sints: TypeScriptSingleType =>
        val fqn = sints.getQualifiedTypeName
        if (generics.contains(fqn)) {
          generics(fqn).apply()
        } else {
          val clsType = JSTypeUtils.createType(sints.getQualifiedTypeName, JSTypeSource.EMPTY)
          val clsGenerics: java.util.List[JSType] = sints.getTypeArguments.map(
            gena => Mt.mergeTypes(GenericRes.parseTypePsi(gena, generics))
              .getOrElse(JSUnknownType.JS_INSTANCE)
          ).toList.asJava
          Some(new JSGenericTypeImpl(JSTypeSource.EMPTY, clsType, clsGenerics))
        }
      case _ => None
    }
  }

  /** (arg: SomeCls<T>) => {} -> returns type of T, knowing the complete type of arg */
  private def getGenericTypeFromArg(argTypePsi: JSTypeDeclaration, getArgt: () => GenTraversableOnce[JSType], generic: String): GenTraversableOnce[JSType] = {
    argTypePsi match {
      case union: TypeScriptUnionOrIntersectionType =>
        union.getTypes.flatMap(subTypePsi =>
          GenericRes.getGenericTypeFromArg(subTypePsi, getArgt, generic))
      case obj: TypeScriptObjectType =>
        val getSubType = () => getArgt().toList.flatMap(t => Mt.getKey(t, None))
        obj.getIndexSignatures.flatMap(sig => GenericRes.getGenericTypeFromArg(sig.getType, getSubType, generic))
      case sints: TypeScriptSingleType =>
        val fqn = sints.getQualifiedTypeName
        if (generic equals fqn) {
          getArgt()
        } else {
          None
        }
      case _ => None
    }
  }

  private def resolveTypeExpr(
     thist: Option[JSType],
     callCtx: IExprCtx,
     caretTypeExpr: TypeScriptType,
     tsFunc: TypeScriptFunctionSignature,
  ): GenTraversableOnce[JSType] = {
    val methGenericPsis = tsFunc.getTypeParameters
    val ifcGenericPsis = Option(tsFunc.getParent)
      .flatMap(objPsi => Option(objPsi.getParent))
      .flatMap(cast[TypeScriptInterface](_)).toList
      .flatMap(ifc => ifc.getTypeParameters)

    val args = tsFunc.getParameters
    val genericsIm: Map[String, () => Array[JSType]] = methGenericPsis
      .flatMap(psi => Option(psi.getName))
      .map(generic => generic -> (() => {
        args.zipWithIndex.flatMap({case (argPsi, i) => Option(argPsi.getTypeElement)
          .flatMap(cast[TypeScriptType](_))
          .filter(argTypeDef => !argTypeDef.equals(caretTypeExpr))
          .toList.flatMap(tst => getGenericTypeFromArg(
            tst, () => callCtx.func().getArg(i), generic)
          )})
      })).toMap

    val genericsMut = collection.mutable.Map(genericsIm.toSeq: _*)

    val ifcGenerics: List[(String, () => Array[JSType])] = thist
      .toList.flatMap(thist => ifcGenericPsis.zipWithIndex
        .flatMap({case (psi, order) => Option(psi.getName)
          .map(generic => (generic, () => {
            Mt.asGeneric(thist, tsFunc.getProject).toList
              .flatMap(gt => gt.getArguments.asScala.lift(order))
              .toArray
          }))
        }))

    // bleeeeh, how many lines for a simple thing...
    ifcGenerics.foreach(t => {
      val (generic, getType) = t
      val old: () => Array[JSType] = genericsMut.getOrElse(generic, () => Array[JSType]())
      genericsMut.remove(generic)
      genericsMut.put(generic, () => (getType().toList ++ old()).toArray)
    })

    parseTypePsi(caretTypeExpr, genericsMut.toMap)
  }
}

/**
 * resolves typescript function signature
 * use this to get return type knowing passed argument types through generics
 */
case class GenericRes(ctx: IExprCtx) {

  def resolveFuncArg(thist: Option[JSType], ctx: IExprCtx, argOrder: Int, tsFunc: TypeScriptFunctionSignature): GenTraversableOnce[JSType] = {
    val result = tsFunc.getParameters.lift(argOrder)
      .flatMap(argPsi => Option(argPsi.getTypeElement))
      .flatMap(cast[TypeScriptType](_)).toList
      .flatMap(caretTypeDef => resolveTypeExpr(thist, ctx, caretTypeDef, tsFunc))
    result
  }

  def resolveFunc(tsFunc: TypeScriptFunctionSignature): GenTraversableOnce[JSType] = {
    Option(tsFunc.getReturnTypeElement)
      .map(caretTypeDef =>
        JSDeepFunctionTypeImpl(tsFunc, ctx.subCtxEmpty().func(), callCtx =>
          resolveTypeExpr(None, callCtx, caretTypeDef, tsFunc)))
  }
}
