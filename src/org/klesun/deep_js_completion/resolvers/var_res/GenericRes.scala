package org.klesun.deep_js_completion.resolvers.var_res

import com.intellij.lang.javascript.psi.ecma6._
import com.intellij.lang.javascript.psi.types._
import com.intellij.lang.javascript.psi.{JSParameterTypeDecorator, JSType, JSTypeUtils}
import org.klesun.deep_js_completion.contexts.IExprCtx
import org.klesun.deep_js_completion.helpers.Mt
import org.klesun.deep_js_completion.resolvers.var_res.GenericRes.resolveTypeExpr
import org.klesun.deep_js_completion.structures
import org.klesun.deep_js_completion.structures.{JSDeepFunctionTypeImpl, JSDeepMultiType}
import org.klesun.lang.DeepJsLang.cast

import scala.collection.GenTraversableOnce
import scala.collection.JavaConverters._
import org.klesun.lang.DeepJsLang._


object GenericRes {
  /** get the type of typePsi replacing generics with actual values */
  private def parseTypePsi(typePsi: JSTypeDeclaration, generics: Map[String, () => Array[JSType]]): GenTraversableOnce[JSType] = {
    typePsi match {
      case arrts: TypeScriptArrayType =>
        val elts = nit(arrts.getType)
          .flatMap(eltPsi => GenericRes.parseTypePsi(eltPsi, generics))
        val arrt = new JSArrayTypeImpl(JSDeepMultiType(elts.mem()), JSTypeSource.EMPTY)
        Some(arrt)
      case arrts: TypeScriptTupleType =>
        val els = arrts.getElements
          .map(eltPsi => GenericRes.parseTypePsi(eltPsi, generics))
          .map(oneElTypes => structures.JSDeepMultiType(oneElTypes.mem()): JSType)

        val tupt = new JSTupleTypeImpl(JSTypeSource.EMPTY, els.toList.asJava, true, -1)
        Some(tupt)
      case functs: TypeScriptFunctionType =>
        val rettypes = parseTypePsi(functs.getReturnTypeElement, generics)
        val rett: JSType = JSDeepMultiType(rettypes.mem())
        val argtypes = functs.getParameters.map(arg => {
          val argtypes = Option(arg.getTypeElement)
            .flatMap(cast[TypeScriptType](_)).itr
            .flatMap(argts => parseTypePsi(argts, generics))
          val argt = JSDeepMultiType(argtypes.mem())
          new JSParameterTypeDecoratorImpl(argt, arg.isOptional, arg.isRest, true)
        })
        val src: JSTypeSource = JSTypeSource.EMPTY
        val argts: java.util.List[JSParameterTypeDecorator] = argtypes.toList
          .map(a => a.asInstanceOf[JSParameterTypeDecorator]).asJava
        val funct = new JSFunctionTypeImpl(src, argts, rett)
        Some(funct)
      case sints: TypeScriptSingleType =>
        val fqn = sints.getQualifiedTypeName
        if (generics.contains(fqn)) {
          generics(fqn).apply()
        } else {
          val clsType = JSTypeUtils.createType(sints.getQualifiedTypeName, JSTypeSource.EMPTY)
          val clsGenerics: java.util.List[JSType] = sints.getTypeArguments.map(
            gena => JSDeepMultiType(GenericRes.parseTypePsi(gena, generics).mem()): JSType
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
        val getSubType = () => getArgt().itr.flatMap(t => Mt.getKey(t, None))
        obj.getIndexSignatures.flatMap(sig => GenericRes.getGenericTypeFromArg(sig.getType, getSubType, generic))
      case sints: TypeScriptSingleType =>
        val fqn = sints.getQualifiedTypeName
        if (generic equals fqn) {
          getArgt()
        } else if ("Iterable" equals fqn) {
          val getSubType = () => getArgt().itr.flatMap(t => Mt.getKey(t, None))
          sints.getTypeArguments.headOption.itr
            .flatMap(eldec => getGenericTypeFromArg(eldec, getSubType, generic))
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
      .flatMap(cast[TypeScriptInterface](_)).itr
      .flatMap(ifc => ifc.getTypeParameters)

    val args = tsFunc.getParameters
    val genericsIm: Map[String, () => Array[JSType]] = methGenericPsis
      .flatMap(psi => Option(psi.getName))
      .map(generic => generic -> (() => {
        args.zipWithIndex.flatMap({case (argPsi, i) => Option(argPsi.getTypeElement)
          .flatMap(cast[TypeScriptType](_))
          .filter(argTypeDef => !argTypeDef.equals(caretTypeExpr))
          .itr.flatMap(tst => getGenericTypeFromArg(
            tst, () => callCtx.func().getArg(i), generic)
          )})
      })).toMap

    val genericsMut = collection.mutable.Map(genericsIm.toSeq: _*)

    val ifcGenerics: It[(String, () => Array[JSType])] = thist
      .itr.flatMap(thist => ifcGenericPsis.zipWithIndex
        .flatMap({case (psi, order) => Option(psi.getName)
          .map(generic => (generic, () => {
            Mt.asGeneric(thist, tsFunc.getProject).itr
              .flatMap(gt => gt.getArguments.asScala.lift(order))
              .toArray
          }))
        }))

    // bleeeeh, how many lines for a simple thing...
    ifcGenerics.foreach(t => {
      val (generic, getType) = t
      val old: () => Array[JSType] = genericsMut.getOrElse(generic, () => Array[JSType]())
      genericsMut.remove(generic)
      genericsMut.put(generic, () => (getType().itr ++ old()).toArray)
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
      .flatMap(cast[TypeScriptType](_)).itr
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
