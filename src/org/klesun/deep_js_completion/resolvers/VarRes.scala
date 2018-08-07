package org.klesun.deep_js_completion.resolvers

import java.util

import com.intellij.lang.javascript.psi.impl.{JSDefinitionExpressionImpl, JSFunctionImpl, JSReferenceExpressionImpl}
import com.intellij.lang.javascript.psi._
import com.intellij.lang.javascript.psi.types._
import com.intellij.psi.{PsiElement, PsiFile}
import com.intellij.psi.impl.source.resolve.reference.impl.providers.{FileReference, FileReferenceSet}
import com.intellij.psi.util.PsiTreeUtil
import org.klesun.deep_js_completion.entry.PathStrGoToDecl
import org.klesun.deep_js_completion.helpers.{ICtx, MultiType}

import scala.collection.JavaConverters._
import org.klesun.lang.Lang._

/**
 * resolves variable type
 */
case class VarRes(ctx: ICtx) {

  private def getInlineFuncArgType(func: JSFunction): Option[JSType] = Option(func.getParent)
    .flatMap(cast[JSArgumentList](_))
    .flatMap(func => Option(func.getParent))
    .flatMap(cast[JSCallExpression](_))
    .flatMap(call => Option(call.getMethodExpression))
    .flatMap(cast[JSReferenceExpressionImpl](_))
    .filter(ref => List("forEach", "map", "filter", "sort", "reduce").contains(ref.getReferencedName))
    .flatMap(ref => Option(ref.getQualifier))
    .flatMap(expr => ctx.findExprType(expr))
    .flatMap(arrt => MultiType.getKey(arrt, None))

  private def ensureFunc(clsT: JSType): Option[JSType] = {
    val funcTs = MultiType.flattenTypes(clsT).map(t => {t match {
      case funcT: JSFunctionTypeImpl => funcT
      case _ => new JSFunctionTypeImpl(JSTypeSource.EMPTY, List[JSParameterTypeDecorator]().asJava, clsT)
    }}: JSFunctionTypeImpl)
    MultiType.mergeTypes(funcTs)
  }

  private def resolveKlesunWhenLoadedSupplierDef(file: PsiFile): Option[JSType] = {
    PsiTreeUtil.findChildrenOfType(file, classOf[JSAssignmentExpression]).asScala
      .filter(assi => assi.getText.startsWith("klesun.whenLoaded")).toList.lift(0)
      .flatMap(assi => Option(assi.getROperand)) // \(^o^)/
      .flatMap(moduleSupplier => ctx.findExprType(moduleSupplier))
  }

  private def resolveRequireJsSupplierDef(file: PsiFile): Option[JSType] = {
    PsiTreeUtil.findChildrenOfType(file, classOf[JSCallExpression]).asScala
      .filter(assi => assi.getText.startsWith("define(")).toList.lift(0)
      .flatMap(call => call.getArguments.lift(1))
      .flatMap(moduleSupplier => ctx.findExprType(moduleSupplier))
  }

  private def resolveRequireJsFormatDef(file: PsiFile): Option[JSType] = {
    val types = List[JSType]() ++
      resolveKlesunWhenLoadedSupplierDef(file) ++
      resolveRequireJsSupplierDef(file)
    MultiType.mergeTypes(types.flatMap(sup => MultiType.getReturnType(sup)))
  }

  private def getKlesunRequiresArgType(func: JSFunction): Option[JSType] = Option(func.getParent)
    .flatMap(cast[JSAssignmentExpression](_))
    .flatMap(assi => Option(assi.getDefinitionExpression))
    .flatMap(defi => Option(defi.getExpression))
    .flatMap(cast[JSReferenceExpressionImpl](_))
    .filter(ref => List("then").contains(ref.getReferencedName))
    .flatMap(ref => Option(ref.getQualifier))
    .flatMap(cast[JSCallExpression](_))
    .filter(call => Option(call.getMethodExpression)
      .map(e => e.getText).getOrElse("").equals("klesun.requires"))
    .flatMap(call => call.getArguments.toList.lift(0))
    .flatMap(arg => PathStrGoToDecl.getReferencedFile(arg))
    .flatMap(file => resolveRequireJsFormatDef(file))
    .flatMap(clsT => ensureFunc(clsT))

  private def resolveArg(para: JSParameter): Option[JSType] = {
    val types = Option(para.getDeclaringFunction)
      .toList.flatMap(func => List[JSType]()
        ++ getInlineFuncArgType(func)
        ++ getKlesunRequiresArgType(func))
    MultiType.mergeTypes(types)
  }

  def first[T](suppliers: (() => Option[T])*): Option[T] = {
    suppliers.iterator.flatMap(s => s())
      .take(1).toList.lift(0)
  }

  def resolve(ref: JSReferenceExpression): Option[JSType] = {
    // TODO: manually support re-assignment, like
    // var someVar = null;
    // ... code
    // someVar = initializeSomething()
    val deepRef = Option(ref.getQualifier)
      .flatMap(qual => ctx.findExprType(qual))
      .flatMap(qualT => {
        val keyTOpt = Option(ref.getReferenceName)
          .map(name => new JSStringLiteralTypeImpl(name, true, JSTypeSource.EMPTY))
        MultiType.getKey(qualT, keyTOpt)
      })

    val briefRef = Option(ref.resolve())
      .flatMap(psi => psi match {
        case para: JSParameter => resolveArg(para)
        case dest: JSVariable => first(() => None
          , () => Option(dest.getInitializer)
            .flatMap(expr => ctx.findExprType(expr))
          , () => Option(dest.getParent)
            .flatMap(cast[JSDestructuringShorthandedProperty](_))
            .flatMap(prop => Option(prop.getParent))
            .flatMap(cast[JSDestructuringObject](_))
            .flatMap(obj => Option(obj.getParent))
            .flatMap(cast[JSDestructuringElement](_))
            .flatMap(obj => Option(obj.getInitializer))
            .flatMap(qual => ctx.findExprType(qual))
            .flatMap(qualT => {
              val keyTOpt = Option(dest.getName)
                .map(name => new JSStringLiteralTypeImpl(name, true, JSTypeSource.EMPTY))
              MultiType.getKey(qualT, keyTOpt)
            })
        )
        case prop: JSProperty => Option(prop.getValue)
          .flatMap(expr => ctx.findExprType(expr))
        case prop: JSDefinitionExpression => Option(prop.getExpression)
          .flatMap(expr => ctx.findExprType(expr))
        case _ =>
          println("Unsupported var declaration - " + psi.getClass + " " + psi.getText)
          None
      })
    MultiType.mergeTypes(deepRef ++ briefRef)
  }
}
