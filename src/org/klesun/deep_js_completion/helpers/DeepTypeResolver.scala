package org.klesun.deep_js_completion.helpers

import com.intellij.lang.javascript.psi.JSRecordType.TypeMember
import com.intellij.lang.javascript.psi.resolve.JSTypeEvaluator
import com.intellij.lang.javascript.psi.types.primitives.{JSBooleanType, JSStringType}
import com.intellij.lang.javascript.psi.types.{JSRecordTypeImpl, JSSimpleRecordTypeImpl, JSTypeContext, JSTypeSource}
import com.intellij.lang.javascript.psi.{JSCallExpression, JSExpression, JSType}
import com.intellij.util.containers.ContainerUtil
import org.klesun.deep_js_completion.resolvers.FuncCallRes

object DeepTypeResolver {
  def resolveIn(expr: JSExpression, ctx: ICtx): Option[JSType] = {
    expr match {
      case call: JSCallExpression => FuncCallRes(ctx).resolve(call)
      case _ => Option(JSTypeEvaluator.getExpressionType(expr).getType)
    }
  }
}
