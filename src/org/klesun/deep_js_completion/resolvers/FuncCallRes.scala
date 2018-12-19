package org.klesun.deep_js_completion.resolvers

import com.intellij.lang.javascript.psi.JSRecordType.TypeMember
import com.intellij.lang.javascript.psi.types.primitives.JSBooleanType
import com.intellij.lang.javascript.psi.types._
import com.intellij.lang.javascript.psi.{JSCallExpression, JSExpression, JSReferenceExpression, JSType}
import com.intellij.util.containers.ContainerUtil
import org.klesun.deep_js_completion.contexts.ICtx
import org.klesun.deep_js_completion.helpers.Mt
import org.klesun.lang.Lang._

import scala.collection.JavaConverters._

/**
 * resolves type of a function call expression like:
 * someVar(arg1, arg2)
 */
case class FuncCallRes(ctx: ICtx) {

  def resolveBuiltInCall(obj: JSExpression, methName: String, args: List[JSExpression]): Option[JSType] = {
    // Unsupported: reduce, concat, shift, pop
    if (List("filter", "sort", "slice", "splice").contains(methName)) {
      ctx.findExprType(obj)
    } else if (List("reduce", "map").contains(methName)) {
      args.lift(0).flatMap(arg => ctx.findExprType(arg))
        .flatMap(funcT => Mt.getReturnType(funcT))
        .map(elT => new JSArrayTypeImpl(elT, JSTypeSource.EMPTY))
    } else {
      None
    }
  }

  def resolve(funcCall: JSCallExpression): Option[JSType] = {
    Option(funcCall.getMethodExpression)
      .flatMap(expr => {
        val definedRts = ctx.findExprType(expr)
          .toList.flatMap(funcT => Mt.getReturnType(funcT))
        val builtInRts = cast[JSReferenceExpression](expr)
          .flatMap(ref => Option(ref.getReferenceName)
            .flatMap(name => Option(ref.getQualifier)
              .flatMap(qual => resolveBuiltInCall(qual, name, funcCall.getArguments.toList))))

        Mt.mergeTypes(definedRts ++ builtInRts)
      })
  }
}
