package org.klesun.deep_js_completion.resolvers

import com.intellij.lang.javascript.psi.JSRecordType.TypeMember
import com.intellij.lang.javascript.psi.types.primitives.JSBooleanType
import com.intellij.lang.javascript.psi.types._
import com.intellij.lang.javascript.psi.{JSCallExpression, JSType}
import com.intellij.util.containers.ContainerUtil
import org.klesun.deep_js_completion.helpers.ICtx
import org.klesun.lang.Lang._

/**
 * resolves type of a function call expression like:
 * someVar(arg1, arg2)
 */
case class FuncCallRes(ctx: ICtx) {
  def resolve(funcCall: JSCallExpression): Option[JSType] = {
    Option(funcCall.getMethodExpression)
      .flatMap(expr => ctx.findExprType(expr))
      .filter(typ => log("zhopa called var " + typ.getClass))
      .flatMap(cast[JSFunctionTypeImpl](_))
      .flatMap(funcT => Option(funcT.getReturnType))

//    val typeMembers = ContainerUtil.newArrayList[TypeMember]
//    val boolT = new JSBooleanType(false, JSTypeSource.EMPTY, JSTypeContext.UNKNOWN)
//    typeMembers.add(new JSRecordTypeImpl.PropertySignatureImpl("someHujProp", boolT, false, funcCall))
//    new JSSimpleRecordTypeImpl(JSTypeSource.EMPTY, typeMembers)
  }
}
