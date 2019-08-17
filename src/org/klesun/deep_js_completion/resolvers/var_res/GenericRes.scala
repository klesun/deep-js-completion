package org.klesun.deep_js_completion.resolvers.var_res

import com.intellij.lang.javascript.psi.ecma6._
import com.intellij.lang.javascript.psi.{JSParameter, JSParameterTypeDecorator, JSType, JSTypeUtils}
import org.klesun.deep_js_completion.contexts.IExprCtx
import org.klesun.deep_js_completion.helpers.Mt
import org.klesun.deep_js_completion.resolvers.var_res.generic_res.{ToGetTypeFromExpr, ToGetTypeOfExpr}
import org.klesun.deep_js_completion.structures.{JSDeepFunctionTypeImpl, JSDeepMultiType}
import org.klesun.lang.DeepJsLang.cast

import scala.collection.GenTraversableOnce
import scala.collection.JavaConverters._
import org.klesun.lang.DeepJsLang._


object GenericRes {
}

/**
 * resolves typescript function signature
 * use this to get return type knowing passed argument types through generics
 */
case class GenericRes(ctx: IExprCtx)
{
  private def resolveTypeExpr(
     thisMit: MemIt[JSType],
     callCtx: IExprCtx,
     caretTypeExpr: TypeScriptType,
     tsFunc: TypeScriptFunctionSignature,
  ): GenTraversableOnce[JSType] = {
    val methGenerics = tsFunc.getTypeParameters
      .flatMap(psi => Option(psi.getName))
    val ifcGenericPsis = Option(tsFunc.getParent)
      .flatMap(objPsi => Option(objPsi.getParent))
      .flatMap(cast[TypeScriptInterface](_)).toList
      .flatMap(ifc => ifc.getTypeParameters)

    val args = tsFunc.getParameters
    val genericsMut = collection.mutable.Map[String, () => MemIt[JSType]]()

    methGenerics.foreach(generic => {
      val getType = () => {
        args.zipWithIndex.flatMap({case (argPsi, i) => nit(argPsi.getTypeElement)
          .flatMap(cast[TypeScriptType](_))
          .filter(argTypeDef => !argTypeDef.equals(caretTypeExpr))
          .itr.flatMap(tst => ToGetTypeFromExpr(ctx, generic, genericsMut).apply(
            tst, () => callCtx.func().getArg(i))
          )}).mem()
      }
      genericsMut.put(generic, getType)
    })
    ifcGenericPsis.zipWithIndex.foreach({case (psi, order) => {
      if (psi.getName != null) {
        val getType = () => thisMit.itr()
          .flatMap(thist => Mt.asGeneric(thist, tsFunc.getProject))
          .flatMap(gt => gt.getArguments.asScala.lift(order)).mem()
        genericsMut.put(psi.getName, getType)
      }
    }})

    ToGetTypeOfExpr(genericsMut).apply(caretTypeExpr)
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
