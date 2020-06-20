package org.klesun.deep_js_completion.resolvers

import java.util

import com.intellij.lang.javascript.documentation.JSDocumentationUtils
import com.intellij.lang.javascript.psi.ecmal4.JSAttributeList
import com.intellij.lang.javascript.psi.{JSFunction, JSParameterTypeDecorator, JSType}
import com.intellij.lang.javascript.psi.ecmal4.JSAttributeList.ModifierType
import com.intellij.lang.javascript.psi.jsdoc.impl.JSDocCommentImpl
import com.intellij.lang.javascript.psi.types.{JSFunctionTypeImpl, JSGenericTypeImpl, JSTypeParser, JSTypeSource}
import org.klesun.deep_js_completion.contexts.IExprCtx
import org.klesun.deep_js_completion.helpers.Mt
import org.klesun.deep_js_completion.resolvers.MainRes.getReturns
import org.klesun.deep_js_completion.structures.JSDeepFunctionTypeImpl
import org.klesun.lang.DeepJsLang.{cast, cnc, nit, substr}
import org.klesun.lang.DeepJsLang._

import scala.collection.GenTraversableOnce

object FuncRes {
  def shouldTypedefBeIgnored(tsFunc: JSFunction): Boolean = {
    // es2015 d.ts has some weird return type - Promise<TResult1 | TResult2>,
    // it results in irrelevant options, so I'm overriding it here
    ("then" equals tsFunc.getName) &&
      List("lib.es2015.promise.d.ts", "lib.es5.d.ts").contains(tsFunc.getContainingFile.getName)
  }
}

case class FuncRes(ctx: IExprCtx) {
  def resolve(func: JSFunction): GenTraversableOnce[JSType] = {
    val isAsync = func.getChildren.flatMap(cast[JSAttributeList](_))
      .exists(lst => lst.hasModifier(ModifierType.ASYNC))
    val docFuncTit = nit(JSDocumentationUtils.findDocComment(func))
      .flatMap(cast[JSDocCommentImpl](_))
      .flatMap(tag => tag.getTags)
      .filter(tag => "return".equals(tag.getName))
      .flatMap(tag => Option(tag.getValue))
      .map(tagVal => tagVal.getText)
      .flatMap(typeText => {
        // the parser does not seem to like {Promise<number>}, it only accepts Promise<number>
        val noBrac = new JSTypeParser(substr(typeText, 1, -1), JSTypeSource.EMPTY).parseParameterType(true)
        nit(noBrac).flatMap(dec => Option(dec.getType))
      })
      .map(rett => new JSFunctionTypeImpl(JSTypeSource.EMPTY,
        new util.ArrayList[JSParameterTypeDecorator](), rett))
    val inferFuncTit = getReturns(func).map(r => {
      JSDeepFunctionTypeImpl(func, callCtx => {
        val rett = callCtx.findExprType(r)
        if (!isAsync) rett else rett.itr
          .flatMap(t => Mt.unwrapPromise(t))
          .map(t => Mt.wrapPromise(t))
      }, Some(ctx.func()))
    })
    val wsTit = Option(func.getReturnType).map(rett => {
      new JSFunctionTypeImpl(JSTypeSource.EMPTY,
        new util.ArrayList[JSParameterTypeDecorator](), rett)
    })
    if (FuncRes.shouldTypedefBeIgnored(func)) {
      None
    } else {
      cnc(docFuncTit, inferFuncTit, wsTit)
    }
  }
}
