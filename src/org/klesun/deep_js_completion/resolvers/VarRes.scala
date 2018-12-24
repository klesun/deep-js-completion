package org.klesun.deep_js_completion.resolvers

import java.util
import java.util.Objects

import com.intellij.lang.javascript.documentation.JSDocumentationUtils
import com.intellij.lang.javascript.psi.JSRecordType.TypeMember
import com.intellij.lang.javascript.psi.impl.{JSDefinitionExpressionImpl, JSFunctionImpl, JSReferenceExpressionImpl}
import com.intellij.lang.javascript.psi._
import com.intellij.lang.javascript.psi.jsdoc.JSDocTag
import com.intellij.lang.javascript.psi.jsdoc.impl.JSDocCommentImpl
import com.intellij.lang.javascript.psi.types.JSRecordMemberSourceFactory.EmptyMemberSource
import com.intellij.lang.javascript.psi.types.JSRecordTypeImpl.{IndexSignatureImpl, PropertySignatureImpl}
import com.intellij.lang.javascript.psi.types._
import com.intellij.psi.{PsiElement, PsiFile, PsiWhiteSpace}
import com.intellij.psi.impl.source.tree.LeafPsiElement
import com.intellij.psi.util.PsiTreeUtil
import org.klesun.deep_js_completion.contexts.{IExprCtx, IFuncCtx}
import org.klesun.deep_js_completion.entry.PathStrGoToDecl
import org.klesun.deep_js_completion.helpers.Mt
import org.klesun.lang.Lang

import scala.collection.JavaConverters._
import org.klesun.lang.Lang._
import org.klesun.deep_js_completion.resolvers.VarRes._
import org.klesun.deep_js_completion.resolvers.var_res.ArgRes

import scala.collection.{GenTraversable, GenTraversableOnce}
import scala.collection.mutable.ListBuffer

object VarRes {

  def findVarUsages(decl: PsiElement, name: String): List[JSReferenceExpression] = {
    val scope: PsiElement = Lang.findParent[JSFunctionExpression](decl)
      .getOrElse(decl.getContainingFile)
    Lang.findChildren[JSReferenceExpression](scope)
      .filter(usage => Objects.equals(usage.getReferenceName, name))
      .filter(usage => !Objects.equals(usage, decl))
      .filter(usage => Objects.equals(decl, usage.resolve()))
  }

  private def findRefUsages(ref: JSReferenceExpression): List[JSReferenceExpression] = {
    Option(ref.resolve()).toList.flatMap(decl => findVarUsages(decl, ref.getReferenceName))
  }
}

/**
 * resolves variable type
 */
case class VarRes(ctx: IExprCtx) {

  private def first[T](suppliers: (() => Option[T])*): Option[T] = {
    suppliers.iterator.flatMap(s => s())
      .take(1).toList.lift(0)
  }

  private def resolveFromUsage(usage: JSReferenceExpression): GenTraversableOnce[JSType] = {
    def resolveParent(parent: PsiElement): GenTraversableOnce[JSType] = {
      parent match {
        case superRef: JSReferenceExpression => first(() => None
          // someVar.push(value)
          , () => Option(superRef.getReferenceName)
            .filter(refName => List("push", "unshift").contains(refName))
            .flatMap(refName => Option(superRef.getParent))
            .flatMap(cast[JSCallExpression](_))
            .flatMap(call => call.getArguments.lift(0))
            .flatMap(value => ctx.findExprType(value))
            .map(elT => new JSArrayTypeImpl(elT, JSTypeSource.EMPTY))
          // someVar.someKey = 123
          , () => Mt.mergeTypes(Option(superRef.getReferencedName).toList
            .flatMap(name => Option(superRef.getParent).toList
              .flatMap(parent => resolveParent(parent))
              .map(valT => new PropertySignatureImpl(name, valT, false, new EmptyMemberSource))
              .map((prop: TypeMember) => new JSRecordTypeImpl(JSTypeSource.EMPTY, List(prop).asJava))))
        )
        // someVar[i] = value
        case indexing: JSIndexedPropertyAccessExpression => Option(indexing.getParent).toList
          .flatMap(parent => resolveParent(parent))
//          .map(valT => new IndexSignatureImpl(ctx.findExprType(indexing.getIndexExpression).orNull, valT, new EmptyMemberSource))
//          .map((prop: TypeMember) => new JSRecordTypeImpl(JSTypeSource.EMPTY, List(prop).asJava))
          // it would actually be more correct to make it "object with any key", not
          // an array... and resolving key string value when we can would be nice too
          .map(elT => new JSArrayTypeImpl(elT, JSTypeSource.EMPTY))
        // var someVar = null;
        // someVar = initializeSomething()
        case usage: JSDefinitionExpression => Option(usage.getParent)
          .flatMap(cast[JSAssignmentExpression](_))
          .flatMap(defi => Option(defi.getROperand))
          .flatMap(expr => ctx.findExprType(expr))
        case _ => List[JSType]()
      }
    }
    Option(usage.getParent).toList
      .flatMap(parent => resolveParent(parent))
  }

  // may be defined in a different file unlike resolveFromUsage()
  private def resolveFromMainDecl(psi: PsiElement): GenTraversableOnce[JSType] = {
    psi match {
      case para: JSParameter => ArgRes(ctx).resolve(para)
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
            Mt.getKey(qualT, keyTOpt)
          })
      )
      case prop: JSProperty => Option(prop.getValue)
        .flatMap(expr => ctx.findExprType(expr))
      case prop: JSDefinitionExpression => Option(prop.getExpression)
        .flatMap(expr => ctx.findExprType(expr))
      case func: JSFunction => Mt.mergeTypes(MainRes.getReturns(func)
        .flatMap(expr => ctx.findExprType(expr))
        .map(rett => new JSFunctionTypeImpl(JSTypeSource.EMPTY, new util.ArrayList[JSParameterTypeDecorator](), rett)))
      case _ =>
        //println("Unsupported var declaration - " + psi.getClass + " " + psi.getText)
        None
    }
  }

  def resolve(ref: JSReferenceExpression): Option[JSType] = {
    val deepRef = Option(ref.getQualifier)
      .flatMap(qual => ctx.findExprType(qual))
      .flatMap(qualT => {
        val keyTOpt = Option(ref.getReferenceName)
          .map(name => new JSStringLiteralTypeImpl(name, true, JSTypeSource.EMPTY))
        Mt.getKey(qualT, keyTOpt)
      })

    val fromUsages = findRefUsages(ref)
      .flatMap(usage => resolveFromUsage(usage))

    val fromMainDecl = Option(ref.resolve()).toList
      .flatMap(psi => resolveFromMainDecl(psi))

    Mt.mergeTypes(deepRef ++ fromUsages ++ fromMainDecl)
  }
}
