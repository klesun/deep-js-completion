package org.klesun.deep_js_completion.resolvers

import java.util

import com.intellij.lang.javascript.psi.impl.{JSDefinitionExpressionImpl, JSFunctionImpl, JSReferenceExpressionImpl}
import com.intellij.lang.javascript.psi._
import com.intellij.lang.javascript.psi.types.{JSFunctionTypeImpl, JSRecordTypeImpl, JSTypeSource}
import com.intellij.psi.PsiElement
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
    .flatMap(file => PsiTreeUtil.findChildrenOfType(file, classOf[JSAssignmentExpression]).asScala
      .filter(assi => assi.getText.startsWith("klesun.whenLoaded")).toList.lift(0))
    .flatMap(assi => Option(assi.getROperand)) // \(^o^)/
    .flatMap(moduleSupplier => ctx.findExprType(moduleSupplier))
    .flatMap(sup => MultiType.getReturnType(sup))
    .flatMap(clsT => ensureFunc(clsT))

  private def resolveArg(para: JSParameter): Option[JSType] = {
    val types = Option(para.getDeclaringFunction)
      .toList.flatMap(func => List[JSType]()
        ++ getInlineFuncArgType(func)
        ++ getKlesunRequiresArgType(func))
    MultiType.mergeTypes(types)
  }

  def resolve(ref: JSReferenceExpression): Option[JSType] = {
    // TODO: manually support re-assignment, like
    // var someVar = null;
    // ... code
    // someVar = initializeSomething()
    Option(ref.resolve())
      .flatMap(psi => psi match {
        case para: JSParameter => resolveArg(para)
        case dest: JSVariable => Option(dest.getInitializer)
          .flatMap(expr => ctx.findExprType(expr))
        case prop: JSProperty => Option(prop.getValue)
          .flatMap(expr => ctx.findExprType(expr))
        case prop: JSDefinitionExpression => Option(prop.getExpression)
          .flatMap(expr => ctx.findExprType(expr))
        case _ =>
          println("Unsupported var declaration - " + psi.getClass)
          None
      })
  }
}
