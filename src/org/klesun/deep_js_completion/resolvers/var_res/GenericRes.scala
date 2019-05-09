package org.klesun.deep_js_completion.resolvers.var_res

import com.intellij.lang.javascript.psi.ecma6._
import com.intellij.lang.javascript.psi.types._
import com.intellij.lang.javascript.psi.{JSParameterTypeDecorator, JSType, JSTypeUtils}
import org.klesun.deep_js_completion.contexts.IExprCtx
import org.klesun.deep_js_completion.helpers.Mt
import org.klesun.deep_js_completion.resolvers.var_res.GenericRes.{parseTypePsi}
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
        } else {
          None
        }
      case _ => None
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

  def resolveFuncArg(thist: MemIt[JSType], ctx: IExprCtx, argOrder: Int, tsFunc: TypeScriptFunctionSignature): GenTraversableOnce[JSType] = {
    val result = tsFunc.getParameters.lift(argOrder)
      .flatMap(argPsi => Option(argPsi.getTypeElement))
      .flatMap(cast[TypeScriptType](_)).itr
      .flatMap(caretTypeDef => resolveTypeExpr(thist, ctx, caretTypeDef, tsFunc))
    result
  }

  def resolveFunc(tsFunc: TypeScriptFunctionSignature, qualMem: MemIt[JSType]): GenTraversableOnce[JSType] = {
    Option(tsFunc.getReturnTypeElement)
      .map(caretTypeDef =>
        JSDeepFunctionTypeImpl(tsFunc, ctx.subCtxEmpty().func(), callCtx => {
          resolveTypeExpr(qualMem, callCtx, caretTypeDef, tsFunc)
        }))
  }
}
