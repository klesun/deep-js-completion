package org.klesun.deep_js_completion.resolvers

import com.intellij.lang.ecmascript6.psi.ES6ExportDefaultAssignment
import com.intellij.lang.javascript.psi.impl.JSReferenceExpressionImpl
import com.intellij.lang.javascript.psi.types.{JSRecordTypeImpl, JSTypeSource}
import com.intellij.lang.javascript.psi._
import com.intellij.psi.PsiFile
import com.intellij.psi.util.PsiTreeUtil
import org.klesun.deep_js_completion.contexts.IExprCtx
import org.klesun.deep_js_completion.helpers.Mt
import org.klesun.lang.DeepJsLang.{It, cast, nit, _}

import scala.collection.GenTraversableOnce
import scala.collection.JavaConverters._
import scala.collection.immutable.List

case class ModuleRes(ctx: IExprCtx) {

  private def resolveRequireJsSupplierDef(file: PsiFile): GenTraversableOnce[JSType] = {
    PsiTreeUtil.findChildrenOfType(file, classOf[JSCallExpression]).asScala
      .find(assi => assi.getText.startsWith("define("))
      .flatMap(call => call.getArguments.lift(1)).itr
      .flatMap(moduleSupplier => ctx.findExprType(moduleSupplier))
  }

  def resolveEs6FormatDef(file: PsiFile): It[JSType] = {
    file.getChildren.itr().flatMap(st => st match {
      case defExp: ES6ExportDefaultAssignment =>
        val valtit = nit(defExp.getExpression)
          .flatMap(ctx.findExprType)
        val prop = Mt.mkProp("default", valtit, Some(defExp))
        val modulet = new JSRecordTypeImpl(JSTypeSource.EMPTY, List(prop).asJava)
        Some(modulet)
      case varSt: JSVarStatement =>
        if (varSt.getText.startsWith("export ")) {
          varSt.getVariables.itr().flatMap(v => {
            val valtit = Option(v.getInitializer).itr().flatMap(i => ctx.findExprType(i))
            val prop = Mt.mkProp(v.getName, valtit, Some(v))
            val modulet = new JSRecordTypeImpl(JSTypeSource.EMPTY, List(prop).asJava)
            Some(modulet)
          })
        } else {
          None
        }
      case _ => None
    })
  }

  def resolveCommonJsFormatDef(file: PsiFile): GenTraversableOnce[JSType] = {
    val types = file.getChildren
      .flatMap(cast[JSExpressionStatement](_))
      .flatMap(_.getChildren)
      .flatMap(cast[JSAssignmentExpression](_)).itr()
      .flatMap(ass => nit(ass.getROperand)
        .flatMap(value => nit(ass.getLOperand)
          .flatMap(vari => {
            val txt = Option(vari.getText).getOrElse("")
            if (txt startsWith "module.exports") {
              ctx.findExprType(value)
            } else if (txt startsWith "exports.") {
              cast[JSDefinitionExpression](vari)
                .filter(ref => Option(vari.getText).exists(txt => txt startsWith "exports."))
                .flatMap(defi => Option(defi.getFirstChild))
                .flatMap(cast[JSReferenceExpressionImpl](_))
                .flatMap(ref => Option(ref.getReferencedName))
                .map(name => Mt.mkProp(name, ctx.findExprType(value), Some(vari)))
                .map(prop => new JSRecordTypeImpl(JSTypeSource.EMPTY, List(prop).asJava))
            } else {
              None
            }
          })
        )
      )
    types
  }

  private def resolveRequireJsFormatDef(file: PsiFile): GenTraversableOnce[JSType] = {
    val types = resolveRequireJsSupplierDef(file)
    types.itr().flatMap(sup => Mt.getReturnType(sup, ctx.subCtxEmpty()))
  }

  def resolve(file: PsiFile): It[JSType] = {
    frs(
      resolveEs6FormatDef(file),
      resolveCommonJsFormatDef(file),
      resolveRequireJsFormatDef(file)
    )
  }
}
