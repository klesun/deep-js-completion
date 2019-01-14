package org.klesun.deep_js_completion.resolvers

import com.intellij.lang.javascript.psi.JSRecordType.TypeMember
import com.intellij.lang.javascript.psi.JSType.TypeTextFormat
import com.intellij.lang.javascript.psi.types.primitives.JSBooleanType
import com.intellij.lang.javascript.psi.types._
import com.intellij.lang.javascript.psi._
import com.intellij.util.containers.ContainerUtil
import org.klesun.deep_js_completion.contexts.IExprCtx
import org.klesun.deep_js_completion.entry.PathStrGoToDecl
import org.klesun.deep_js_completion.helpers.Mt
import org.klesun.deep_js_completion.resolvers.var_res.ArgRes
import org.klesun.deep_js_completion.structures.{EInstType, JSDeepModuleTypeImpl}
import org.klesun.lang.Lang._

import scala.collection.JavaConverters._

/**
 * resolves type of a function call expression like:
 * someVar(arg1, arg2)
 */
case class FuncCallRes(ctx: IExprCtx) {

  def resolveBuiltInMethCall(obj: JSExpression, methName: String, args: List[JSExpression]): Option[JSType] = {
    // Unsupported: reduce, concat, shift, pop
    if (List("filter", "sort", "slice", "splice").contains(methName)) {
      ctx.findExprType(obj)
    } else if (List("concat").contains(methName)) {
      val types = (List(obj) ++ args).flatMap(expr => ctx.findExprType(expr))
      Mt.mergeTypes(types)
    } else if (List("map").contains(methName)) {
      args.lift(0).flatMap(arg => ctx.findExprType(arg))
        .flatMap(funcT => Mt.getReturnType(funcT, ctx.subCtxEmpty()))
        .map(elT => new JSArrayTypeImpl(elT, JSTypeSource.EMPTY))
    } else if (List("reduce").contains(methName)) {
      args.lift(0).flatMap(arg => ctx.findExprType(arg))
        .flatMap(funcT => Mt.getReturnType(funcT, ctx.subCtxEmpty()))
    } else if ((obj.getText equals "Object") && (methName equals "assign")) {
      // IDEA actually has the built-in function generic return type mapping, but I'm
      // not able to get the info (getReturnType returns AnyType instead of A & B)
      Mt.mergeTypes(args.flatMap(arg => ctx.findExprType(arg)))
    } else if (methName equals "then") {
      args.lift(0).flatMap(arg => ctx.findExprType(arg))
          .flatMap(funcT => Mt.getReturnType(funcT, ctx.subCtxEmpty()))
          .map(value => Mt.unwrapPromise(value).getOrElse(value))
          .map(value => Mt.wrapPromise(value))
    } else if ((methName equals "catch") || (methName equals "finally")) {
      ctx.findExprType(obj) // Promise .catch(), could actually add the type from callback, but nah for now
    } else {
      None
    }
  }

  def resolveBuiltInFuncCall(funcName: String, args: List[JSExpression]): Option[JSType] = {
    if (List("require").contains(funcName)) {
      val types = args.lift(0).toList.flatMap(arg => {
        PathStrGoToDecl.getReferencedFile(arg)
          .flatMap(file => ArgRes(ctx).resolveCommonJsFormatDef(file)) ++
        cast[JSLiteralExpression](arg)
          .map(lit => JSDeepModuleTypeImpl(lit.getValue + "", EInstType.Required))
      })
      Mt.mergeTypes(types)
    } else {
      None
    }
  }

  def resolve(funcCall: JSCallExpression): Option[JSType] = {
    Option(funcCall.getMethodExpression)
      .flatMap(expr => {
        val definedRts = ctx.findExprType(expr)
          .toList.flatMap(funcT => Mt.getReturnType(funcT, ctx.subCtxDirect(funcCall)))
        val builtInRts = cast[JSReferenceExpression](expr)
          .flatMap(ref => Option(ref.getReferenceName)
            .flatMap(name => Option(ref.getQualifier) match {
              case Some(qual) => resolveBuiltInMethCall(qual, name, funcCall.getArguments.toList)
              case None => resolveBuiltInFuncCall(name, funcCall.getArguments.toList)
            }))

        Mt.mergeTypes(definedRts ++ builtInRts)
      })
  }
}
