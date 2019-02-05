package org.klesun.deep_js_completion.resolvers

import com.intellij.lang.javascript.psi._
import com.intellij.lang.javascript.psi.types._
import org.klesun.deep_js_completion.contexts.IExprCtx
import org.klesun.deep_js_completion.entry.PathStrGoToDecl
import org.klesun.deep_js_completion.helpers.Mt
import org.klesun.deep_js_completion.resolvers.var_res.ArgRes
import org.klesun.deep_js_completion.structures.{EInstType, JSDeepModuleTypeImpl}
import org.klesun.lang.Lang._

import scala.collection.GenTraversableOnce

/**
 * resolves type of a function call expression like:
 * someVar(arg1, arg2)
 */
case class FuncCallRes(ctx: IExprCtx) {

  def resolveBuiltInMethCall(obj: JSExpression, methName: String, args: List[JSExpression]): GenTraversableOnce[JSType] = {
    // Unsupported: reduce, concat, shift, pop
    if (List("filter", "sort", "slice", "splice").contains(methName)) {
      ctx.findExprType(obj)
    } else if (List("concat").contains(methName)) {
      val types = (List(obj) ++ args).flatMap(expr => ctx.findExprType(expr))
      types
    } else if (List("map").contains(methName)) {
      args.lift(0).itr.flatMap(arg => ctx.findExprType(arg))
        .flatMap(funcT => Mt.getReturnType(funcT, ctx.subCtxEmpty()))
        .map(elT => new JSArrayTypeImpl(elT, JSTypeSource.EMPTY))
    } else if (List("reduce").contains(methName)) {
      args.lift(0).itr.flatMap(arg => ctx.findExprType(arg))
        .flatMap(funcT => Mt.getReturnType(funcT, ctx.subCtxEmpty()))
    } else if ((obj.getText equals "Object") && (methName equals "assign")) {
      // IDEA actually has the built-in function generic return type mapping, but I'm
      // not able to get the info (getReturnType returns AnyType instead of A & B)
      args.flatMap(arg => ctx.findExprType(arg))
    } else if (methName equals "then") {
      args.lift(0).itr.flatMap(arg => ctx.findExprType(arg))
          .flatMap(funcT => Mt.getReturnType(funcT, ctx.subCtxEmpty()))
          .flatMap(value => Mt.unwrapPromise(value))
          .map(value => Mt.wrapPromise(value))
    } else if ((methName equals "catch") || (methName equals "finally")) {
      ctx.findExprType(obj) // Promise .catch(), could actually add the type from callback, but nah for now
    } else {
      None
    }
  }

  def resolveBuiltInFuncCall(funcName: String, args: List[JSExpression]): GenTraversableOnce[JSType] = {
    if (List("require").contains(funcName)) {
      val types = args.lift(0).itr.flatMap(arg => {
        PathStrGoToDecl.getReferencedFile(arg).itr
          .flatMap(file => ArgRes(ctx.subCtxEmpty()).resolveCommonJsFormatDef(file)) ++
        cast[JSLiteralExpression](arg)
          .map(lit => JSDeepModuleTypeImpl(lit.getValue + "", EInstType.Required))
      })
      types
    } else {
      None
    }
  }

  def resolve(funcCall: JSCallExpression): GenTraversableOnce[JSType] = {
    Option(funcCall.getMethodExpression).itr
      .flatMap(expr => {
        val definedRts = ctx.findExprType(expr)
          .itr.flatMap(funcT => Mt.getReturnType(funcT, ctx.subCtxDirect(funcCall)))
        val builtInRts = cast[JSReferenceExpression](expr).itr
          .flatMap(ref => nit(ref.getReferenceName)
            .flatMap(name => Option(ref.getQualifier) match {
              case Some(qual) => resolveBuiltInMethCall(qual, name, funcCall.getArguments.toList)
              case None => resolveBuiltInFuncCall(name, funcCall.getArguments.toList)
            }))

        definedRts ++ builtInRts
      })
  }
}
