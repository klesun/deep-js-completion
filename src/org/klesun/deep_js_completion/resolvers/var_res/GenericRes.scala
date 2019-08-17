package org.klesun.deep_js_completion.resolvers.var_res

import com.intellij.lang.javascript.psi.ecma6._
import com.intellij.lang.javascript.psi.ecma6.impl.{TypeScriptArrayTypeImpl, TypeScriptTupleTypeImpl}
import com.intellij.lang.javascript.psi.types._
import com.intellij.lang.javascript.psi.{JSParameter, JSParameterTypeDecorator, JSType, JSTypeUtils}
import org.klesun.deep_js_completion.contexts.IExprCtx
import org.klesun.deep_js_completion.helpers.Mt
import org.klesun.deep_js_completion.resolvers.var_res.GenericRes.parseTypePsi
import org.klesun.deep_js_completion.structures
import org.klesun.deep_js_completion.structures.{JSDeepFunctionTypeImpl, JSDeepMultiType}
import org.klesun.lang.DeepJsLang.cast

import scala.collection.GenTraversableOnce
import scala.collection.JavaConverters._
import org.klesun.lang.DeepJsLang._


object GenericRes {
  /** get the type of typePsi replacing generics with actual values */
  private def parseTypePsi(
    typePsi: JSTypeDeclaration,
    generics: collection.mutable.Map[String, () => It[JSType]]
  ): GenTraversableOnce[JSType] = {
    typePsi match {
      case arrts: TypeScriptArrayType =>
        val elts = nit(arrts.getType)
          .flatMap(eltPsi => GenericRes.parseTypePsi(eltPsi, generics))
        val arrt = new JSArrayTypeImpl(JSDeepMultiType(elts.mem()), JSTypeSource.EMPTY)
        Some(arrt)
      case interts: TypeScriptUnionOrIntersectionType => interts.getTypes.itr()
          .flatMap(eltPsi => GenericRes.parseTypePsi(eltPsi, generics))
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
      case _ =>
        //Console.println("unsupported return generic kind: " + typePsi.getClass + " " + typePsi.getText)
        None
    }
  }
}

/**
 * resolves typescript function signature
 * use this to get return type knowing passed argument types through generics
 */
case class GenericRes(ctx: IExprCtx) {

  /** (arg: SomeCls<T>) => {} -> returns type of T, knowing the complete type of arg */
  private def getGenericTypeFromArg(argTypePsi: JSTypeDeclaration, getArgt: () => GenTraversableOnce[JSType], generic: String): GenTraversableOnce[JSType] = {
    argTypePsi match {
      case union: TypeScriptUnionOrIntersectionType =>
        union.getTypes.itr().flatMap(subTypePsi =>
          getGenericTypeFromArg(subTypePsi, getArgt, generic))
      case obj: TypeScriptObjectType =>
        val getSubType = () => getArgt().itr.flatMap(t => ctx.mt().getKey(t, None))
        obj.getIndexSignatures.itr().flatMap(sig => getGenericTypeFromArg(sig.getType, getSubType, generic))
      case sints: TypeScriptSingleType =>
        val fqn = sints.getQualifiedTypeName
        if (generic equals fqn) {
          getArgt()
        } else if ("Iterable" equals fqn) {
          val getSubType = () => getArgt().itr.flatMap(t => ctx.mt().getKey(t, None))
          sints.getTypeArguments.headOption.itr
            .flatMap(eldec => getGenericTypeFromArg(eldec, getSubType, generic))
        } else if ("ArrayIterator" equals fqn) {
          // lodash; possibly should resolve these custom lib types rather than hardcode them...
          // getArgt() holds the type of the passed function
          val getArgMit = () => getArgt().mem()
          val getArgType = () => {
            // should probably store argument declaration types in deep js function as well... but not sure
            None
            //getArgMit().itr.flatMap(t => Mt.getArgType(t, ctx, 0))
          }: GenTraversableOnce[JSType]
          val getRetType = () => getArgMit().itr.flatMap(t => Mt.getReturnType(t, ctx))
          cnc(
            sints.getTypeArguments.lift(0).itr()
              .flatMap(argdec => getGenericTypeFromArg(argdec, getArgType, generic)),
            sints.getTypeArguments.lift(1).itr()
              .flatMap(retdec => getGenericTypeFromArg(retdec, getRetType, generic)),
          )
        } else if (
          ("PromiseLike" equals fqn) ||
          ("Promise" equals fqn) ||
          ("Bluebird" equals fqn)
        ) {
          val getSubType = () => getArgt().itr.flatMap(t => Mt.unwrapPromise(t))
          sints.getTypeArguments.headOption.itr
            .flatMap(eldec => getGenericTypeFromArg(eldec, getSubType, generic))
        } else {
          //Console.println("Unsupported generic type expr arg class - " + fqn + " - " + argTypePsi)
          None
        }
      case arrts: TypeScriptArrayTypeImpl =>
        val getSubType = () => getArgt().itr.flatMap(t => ctx.mt().getKey(t, None))
        Option(arrts.getType).itr
          .flatMap(eldec => getGenericTypeFromArg(eldec, getSubType, generic))
      case tupts: TypeScriptTupleTypeImpl =>
        val getSubType = (order: Int) => {
          val keyt = new JSStringLiteralTypeImpl(order + "", true, JSTypeSource.EMPTY)
          getArgt().itr().flatMap(t => ctx.mt().getKey(t, Some(keyt)))
        }
        tupts.getElements.zipWithIndex.itr().flatMap({
          case (typePsi, i) => getGenericTypeFromArg(typePsi, () => getSubType(i), generic)
        })
      case _ =>
        //Console.println("Unsupported generic type expr arg kind - " + argTypePsi.getClass + " - " + argTypePsi)
        None
    }
  }

  private def resolveTypeExpr(
     thisMit: MemIt[JSType],
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
    val genericsIm: Map[String, () => It[JSType]] = methGenericPsis
      .flatMap(psi => Option(psi.getName))
      .map(generic => generic -> (() => {
        args.zipWithIndex.flatMap({case (argPsi, i) => nit(argPsi.getTypeElement)
          .flatMap(cast[TypeScriptType](_))
          .filter(argTypeDef => !argTypeDef.equals(caretTypeExpr))
          .itr.flatMap(tst => getGenericTypeFromArg(
            tst, () => callCtx.func().getArg(i), generic)
          )}).itr()
      })).toMap

    val genericsMut = collection.mutable.Map(genericsIm.toSeq: _*)
    ifcGenericPsis.zipWithIndex.foreach({case (psi, order) => {
      if (psi.getName != null) {
        genericsMut.put(psi.getName, () => thisMit.itr()
          .flatMap(thist => Mt.asGeneric(thist, tsFunc.getProject))
          .flatMap(gt => gt.getArguments.asScala.lift(order)))
      }
    }})

    parseTypePsi(caretTypeExpr, genericsMut)
  }

  def resolveFuncArg(thist: MemIt[JSType], ctx: IExprCtx, argPsi: JSParameter, tsFunc: TypeScriptFunctionSignature): GenTraversableOnce[JSType] = {
    val result = Option(argPsi.getTypeElement)
      .flatMap(cast[TypeScriptType](_)).itr
      .flatMap(caretTypeDef => resolveTypeExpr(thist, ctx, caretTypeDef, tsFunc))
    result
  }

  def resolveFunc(tsFunc: TypeScriptFunctionSignature, qualMem: MemIt[JSType]): GenTraversableOnce[JSType] = {
    Option(tsFunc.getReturnTypeElement)
      .map(caretTypeDef =>
        JSDeepFunctionTypeImpl(tsFunc, callCtx => {
          resolveTypeExpr(qualMem, callCtx, caretTypeDef, tsFunc)
        }, Some(ctx.subCtxEmpty().func())))
  }
}
