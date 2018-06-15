package org.klesun.deep_js_completion.resolvers

import com.intellij.lang.javascript.psi.JSRecordType.TypeMember
import com.intellij.lang.javascript.psi.types.primitives.JSBooleanType
import com.intellij.lang.javascript.psi.types._
import com.intellij.lang.javascript.psi.{JSCallExpression, JSType}
import com.intellij.util.containers.ContainerUtil
import org.klesun.deep_js_completion.helpers.{ICtx, MultiType}
import org.klesun.lang.Lang._

import scala.collection.JavaConverters._

/**
 * resolves type of a function call expression like:
 * someVar(arg1, arg2)
 */
case class FuncCallRes(ctx: ICtx) {
  def resolve(funcCall: JSCallExpression): Option[JSType] = {
    val rTypes = Option(funcCall.getMethodExpression)
      .flatMap(expr => ctx.findExprType(expr))
      .toList
      .flatMap(funcT => funcT match {
        case funcT: JSFunctionTypeImpl => List(funcT)
        case mt: JSContextualUnionTypeImpl => mt.getTypes.asScala
            .flatMap(cast[JSFunctionTypeImpl](_))
        case _ => List()
      })
      .flatMap(funcT => Option(funcT.getReturnType))
    MultiType.mergeTypes(rTypes)
  }
}
