package org.klesun.deep_js_completion.resolvers.var_res.generic_res

import com.intellij.lang.javascript.psi.{JSParameterTypeDecorator, JSType, JSTypeUtils}
import com.intellij.lang.javascript.psi.ecma6._
import com.intellij.lang.javascript.psi.types._
import org.klesun.deep_js_completion.structures
import org.klesun.deep_js_completion.structures.JSDeepMultiType
import org.klesun.lang.DeepJsLang.{MemIt, cast, nit}

import scala.collection.GenTraversableOnce
import scala.collection.JavaConverters._
import org.klesun.lang.DeepJsLang._

case class ToGetTypeOfResult(
  generics: collection.mutable.Map[String, () => MemIt[JSType]],
) {
  def applyToFunc(functs: TypeScriptFunctionType): GenTraversableOnce[JSType] = {
    val rettypes = apply(functs.getReturnTypeElement)
    val rett: JSType = JSDeepMultiType(rettypes.mem())
    val argtypes = functs.getParameters.map(arg => {
      val argtypes = Option(arg.getTypeElement)
        .flatMap(cast[TypeScriptType](_)).itr
        .flatMap(argts => apply(argts))
      val argt = JSDeepMultiType(argtypes.mem())
      new JSParameterTypeDecoratorImpl(argt, arg.isOptional, arg.isRest, true)
    })
    val src: JSTypeSource = JSTypeSource.EMPTY
    val argts: java.util.List[JSParameterTypeDecorator] = argtypes.toList
      .map(a => a.asInstanceOf[JSParameterTypeDecorator]).asJava
    val funct = new JSFunctionTypeImpl(src, argts, rett)
    Some(funct)
  }

  /**
   * get the type of typePsi replacing generics with actual values
   */
  def apply(typePsi: JSTypeDeclaration): GenTraversableOnce[JSType] = {
    typePsi match {
      case arrts: TypeScriptArrayType =>
        val elts = nit(arrts.getType)
          .flatMap(eltPsi => apply(eltPsi))
        val arrt = new JSArrayTypeImpl(JSDeepMultiType(elts.mem()), JSTypeSource.EMPTY)
        Some(arrt)
      case interts: TypeScriptUnionOrIntersectionType => interts.getTypes.itr()
          .flatMap(eltPsi => apply(eltPsi))
      case arrts: TypeScriptTupleType =>
        val els = arrts.getElements
          .map(eltPsi => apply(eltPsi))
          .map(oneElTypes => structures.JSDeepMultiType(oneElTypes.mem()): JSType)

        val tupt = new JSTupleTypeImpl(JSTypeSource.EMPTY, els.toList.asJava, true, -1)
        Some(tupt)
      case functs: TypeScriptFunctionType => applyToFunc(functs)
      case sints: TypeScriptSingleType =>
        val fqn = sints.getQualifiedTypeName
        if (generics.contains(fqn)) {
          generics(fqn).apply()
        } else {
          val clsType = JSTypeUtils.createType(sints.getQualifiedTypeName, JSTypeSource.EMPTY)
          val clsGenerics: java.util.List[JSType] = sints.getTypeArguments.map(
            gena => JSDeepMultiType(apply(gena).mem()): JSType
          ).toList.asJava
          Some(new JSGenericTypeImpl(JSTypeSource.EMPTY, clsType, clsGenerics))
        }
      case _ =>
        //Console.println("unsupported return generic kind: " + typePsi.getClass + " " + typePsi.getText)
        None
    }
  }
}
