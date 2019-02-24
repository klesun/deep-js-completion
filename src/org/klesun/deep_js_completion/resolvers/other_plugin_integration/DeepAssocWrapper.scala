package org.klesun.deep_js_completion.resolvers.other_plugin_integration

import com.intellij.lang.javascript.psi.JSRecordType.PropertySignature
import com.intellij.lang.javascript.psi.types.JSRecordTypeImpl.IndexSignatureImpl
import com.intellij.lang.javascript.psi.types._
import com.intellij.lang.javascript.psi.types.primitives.{JSBooleanType, JSNumberType, JSStringType}
import com.intellij.lang.javascript.psi.{JSFunction, JSType}
import com.intellij.psi.PsiElement
import com.jetbrains.php.lang.psi.resolve.types.PhpType
import org.klesun.deep_assoc_completion.helpers.{Mt => PhpMt}
import org.klesun.deep_assoc_completion.resolvers.other_plugin_integration.DeepAssocApi
import org.klesun.deep_assoc_completion.structures.{DeepType, KeyType}
import org.klesun.deep_js_completion.completion_providers.PropNamePvdr
import org.klesun.deep_js_completion.contexts.{ExprCtx, FuncCtx, SearchCtx}
import org.klesun.deep_js_completion.helpers.{Mt => JsMt}
import org.klesun.deep_js_completion.resolvers.other_plugin_integration.DeepAssocWrapper._
import org.klesun.deep_js_completion.resolvers.var_res.ArgRes
import org.klesun.deep_js_completion.structures.{DeepIndexSignatureImpl, JSDeepFunctionTypeImpl}
import org.klesun.lang.DeepJsLang._

import scala.collection.JavaConverters._

object DeepAssocWrapper {
  def jsToPhp(jst: JSType, psi: PsiElement, depth: Integer = 0, occs: Set[JSType] = Set()): DeepType = {
    val nextOccs = occs ++ Set(jst)
    if (occs.contains(jst)) {
      // when some of string members are of type string themselves
      new DeepType(psi, PhpType.UNSET)
    } else {
      jst match {
        case lit: JSPrimitiveLiteralType[Any] =>
          new DeepType(psi, PhpType.STRING, lit.getLiteral + "")
        case strt: JSStringType => new DeepType(psi, PhpType.STRING)
        case numt: JSNumberType => new DeepType(psi, PhpType.NUMBER)
        case boot: JSBooleanType => new DeepType(psi, PhpType.BOOLEAN)
        case boot: JSFunction => new DeepType(psi, PhpType.CALLABLE)
        case boot: JSDeepFunctionTypeImpl => new DeepType(psi, PhpType.CALLABLE)
        case arrt: JSArrayType =>
          val phpt = new DeepType(psi, PhpType.ARRAY)
          val valTit: java.util.Iterator[DeepType] = JsMt.flattenTypes(arrt.getType)
            .map(t => jsToPhp(t, psi, depth + 1, nextOccs))
            .itr().allowEndHasNext().asJava
          phpt.addKey(KeyType.integer(psi))
            .addType(() => new PhpMt(() => valTit))
          phpt
        case tupt: JSTupleType =>
          val phpt = new DeepType(psi, PhpType.ARRAY)
          val jsValTit = tupt.getTypes.asScala.flatMap(t => JsMt.flattenTypes(t))
          val valTit: java.util.Iterator[DeepType] = jsValTit
            .map(t => jsToPhp(t, psi, depth + 1, nextOccs))
            .itr().allowEndHasNext().asJava
          phpt.addKey(KeyType.integer(psi))
            .addType(() => new PhpMt(() => valTit))
          phpt
        case _ =>
          val mems = PropNamePvdr.getMems(jst, psi.getProject)
          val phpt = new DeepType(psi, PhpType.ARRAY)
          mems.foreach(mem => {
            var kpsi = psi
            var keyt: JSType = JSUnknownType.JS_INSTANCE
            var valt: JSType = JSUnknownType.JS_INSTANCE
            mem match {
              case deep: DeepIndexSignatureImpl =>
                kpsi = deep.psi.getOrElse(psi)
                keyt = deep.keyt
                valt = deep.valt
              case idx: IndexSignatureImpl =>
                keyt = idx.getMemberParameterType
                valt = idx.getMemberType
              case prop: PropertySignature =>
                keyt = new JSStringLiteralTypeImpl(prop.getMemberName, true, JSTypeSource.EMPTY)
                valt = prop.getType
              case _ =>
            }
            val keyTit = JsMt.flattenTypes(keyt)
              .map(t => jsToPhp(t, kpsi, depth + 1, nextOccs))
              .itr().allowEndHasNext().asJava
            val kt = KeyType.mt(() => keyTit, kpsi)
            val valTit: java.util.Iterator[DeepType] = JsMt.flattenTypes(valt)
              .map(t => jsToPhp(t, psi, depth + 1, nextOccs))
              .itr().allowEndHasNext().asJava
            phpt.addKey(kt)
              .addType(() => new PhpMt(() => valTit))
          })
          phpt
      }
    }
  }
}

class DeepAssocWrapper {
  def registerDeepTypeProviders(): Unit = {
    Console.println("begin registerDeepTypeProviders()")

//    val test = Mkt.str(null, "asdasd")
//    Console.println("test obj from deep-assoc - " + test)

    DeepAssocApi.inst().addCustomDocParser("deep-js-completion", (str: String, psi: PsiElement/*, phpCtx: IExprCtx*/) => {
      // should probably use a global search ctx for such stuff, so that caching and depth limits worked
      val jsSearch = new SearchCtx(project=Option(psi.getProject))
      val funcCtx = FuncCtx(jsSearch)
      val jsCtx = ExprCtx(funcCtx, psi, 0)

      val jsTit = ArgRes(jsCtx).parseDocExpr(psi, str).itr()
      () => jsTit
        .flatMap(t => JsMt.flattenTypes(t))
        .map((jst: JSType) => jsToPhp(jst, psi))
        .itr().allowEndHasNext().asJava
    })

    Console.println("end registerDeepTypeProviders()")
//    try {
//      val deepAssocPath = "org.klesun.deep_assoc_completion.resolvers.other_plugin_integration.DeepAssocApi"
//      val deepAssoc = Class.forName(deepAssocPath)
//    } catch {
//      case exc: Throwable => {
//        Console.println("Failed to pass js var phpdoc parser to deep-assoc plugin")
//      }
//    }
  }
}
