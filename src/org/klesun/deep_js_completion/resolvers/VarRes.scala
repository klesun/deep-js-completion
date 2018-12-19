package org.klesun.deep_js_completion.resolvers

import java.util
import java.util.Objects

import com.intellij.lang.javascript.dialects.JSDialectSpecificHandlersFactory
import com.intellij.lang.javascript.documentation.JSDocumentationUtils
import com.intellij.lang.javascript.psi.JSRecordType.TypeMember
import com.intellij.lang.javascript.psi.impl.{JSDefinitionExpressionImpl, JSFunctionImpl, JSReferenceExpressionImpl}
import com.intellij.lang.javascript.psi._
import com.intellij.lang.javascript.psi.jsdoc.JSDocTag
import com.intellij.lang.javascript.psi.jsdoc.impl.JSDocCommentImpl
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil
import com.intellij.lang.javascript.psi.types.JSRecordMemberSourceFactory.EmptyMemberSource
import com.intellij.lang.javascript.psi.types.JSRecordTypeImpl.PropertySignatureImpl
import com.intellij.lang.javascript.psi.types._
import com.intellij.psi.impl.source.resolve.ResolveCache.PolyVariantResolver
import com.intellij.psi.{PsiElement, PsiFile, PsiWhiteSpace}
import com.intellij.psi.impl.source.resolve.reference.impl.providers.{FileReference, FileReferenceSet}
import com.intellij.psi.impl.source.tree.LeafPsiElement
import com.intellij.psi.util.PsiTreeUtil
import org.klesun.deep_js_completion.contexts.ICtx
import org.klesun.deep_js_completion.entry.PathStrGoToDecl
import org.klesun.deep_js_completion.helpers.Mt
import org.klesun.lang.Lang

import scala.collection.JavaConverters._
import org.klesun.lang.Lang._

import scala.collection.{GenTraversable, GenTraversableOnce}
import scala.collection.mutable.ListBuffer

/**
 * resolves variable type
 */
case class VarRes(ctx: ICtx) {

  private def getInlineFuncArgType(func: JSFunction): GenTraversableOnce[JSType] = Option(func.getParent)
    .flatMap(cast[JSArgumentList](_))
    .flatMap(func => Option(func.getParent))
    .flatMap(cast[JSCallExpression](_))
    .flatMap(call => Option(call.getMethodExpression))
    .flatMap(cast[JSReferenceExpressionImpl](_))
    .toList
    .flatMap(ref =>
      Option(ref.getQualifier)
        .filter(expr => List("forEach", "map", "filter", "sort", "reduce").contains(ref.getReferencedName))
        .flatMap(expr => ctx.findExprType(expr))
        .flatMap(arrt => Mt.getKey(arrt, None))
      ++
      Option(ref.getQualifier)
        .filter(expr => List("then").contains(ref.getReferencedName))
        .flatMap(expr => ctx.findExprType(expr)).toList
        .flatMap(promiset => Mt.getPromiseValue(promiset))
    )

  private def ensureFunc(clsT: JSType): Option[JSType] = {
    val funcTs = Mt.flattenTypes(clsT).map(t => {t match {
      case funcT: JSFunctionTypeImpl => funcT
      case _ => new JSFunctionTypeImpl(JSTypeSource.EMPTY, List[JSParameterTypeDecorator]().asJava, clsT)
    }}: JSFunctionTypeImpl)
    Mt.mergeTypes(funcTs)
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

  def resolveCommonJsFormatDef(file: PsiFile): Option[JSType] = {
    val types = file.getChildren
      .flatMap(cast[JSExpressionStatement](_))
      .flatMap(_.getChildren)
      .flatMap(cast[JSAssignmentExpression](_))
      .flatMap(ass => Option(ass.getROperand)
        .flatMap(value => Option(ass.getLOperand)
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
                .flatMap(name => ctx.findExprType(value)
                  .map(valT => new PropertySignatureImpl(name, valT, false, new EmptyMemberSource)))
                .map((prop: TypeMember) => new JSRecordTypeImpl(JSTypeSource.EMPTY, List(prop).asJava))
            } else {
              None
            }
          })
        )
      )
    Mt.mergeTypes(types)
  }

  private def resolveRequireJsFormatDef(file: PsiFile): Option[JSType] = {
    val types = List[JSType]() ++
      resolveKlesunWhenLoadedSupplierDef(file) ++
      resolveRequireJsSupplierDef(file)
    Mt.mergeTypes(types.flatMap(sup => Mt.getReturnType(sup)))
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

  private def getDocTagComment(docTag: JSDocTag) = {
    var next = docTag.getNextSibling
    val tokens = new ListBuffer[PsiElement]
    while (next != null && (
      next.isInstanceOf[LeafPsiElement] ||
      next.isInstanceOf[PsiWhiteSpace]
    )) {
      tokens.append(next)
      next = next.getNextSibling
    }
    tokens.map(t => t.getText).mkString("")
      .replaceAll("""\n\s*\* """, "\n")
      .replaceAll("""\*\/$""", "")
  }

  private def findVarDecl(caretPsi: PsiElement, varName: String): Option[JSType] = {
    var scope: Option[PsiElement] = None
    var stmts = List[JSStatement]()
    val funcOpt = Lang.findParent[JSBlockStatement](caretPsi)
    val fileOpt = Lang.findParent[JSFile](caretPsi)
    if (funcOpt.isDefined) {
      scope = funcOpt
      stmts = funcOpt.get.getStatements.toList
    } else if (fileOpt.isDefined) {
      scope = fileOpt
      stmts = fileOpt.get.getStatements.toList
        .flatMap(cast[JSStatement](_))
    }
    val types = scope.toList.flatMap(b => stmts
      .flatMap(st => st match {
        case varSt: JSVarStatement =>
          varSt.getDeclarations
            .filter(own => varName.equals(own.getName))
            .map(own => own.getInitializer)
            .flatMap(expr => ctx.findExprType(expr))
        case func: JSFunctionDeclaration =>
          if (varName.equals(func.getName)) {
            val rts = MainRes.getReturns(func)
              .flatMap(ret => ctx.findExprType(ret))
            val rt = Mt.mergeTypes(rts).getOrElse(JSUnknownType.JS_INSTANCE)
            Some(new JSFunctionTypeImpl(JSTypeSource.EMPTY, new util.ArrayList, rt))
          } else {
            None
          }
        case _ => None
      })
      .++(findVarDecl(b, varName))
    )
    Mt.mergeTypes(types)
  }

  private def parseDocExpr(caretPsi: PsiElement, expr: String): Iterable[JSType] = {
    Option(null)
      .orElse("""^\s*=\s*(\w+)(\([^\)]*\)|)\s*$""".r.findFirstMatchIn(expr)
        .flatMap(found => {
          val varName = found.group(1)
          val isFuncCall = !found.group(2).equals("")
          findVarDecl(caretPsi, varName)
            .flatMap(t => if (isFuncCall) Mt.getReturnType(t) else Some(t))
        }))
      .orElse("""^\s*=\s*from\('([^']+)'\)(\([^\)]*\)|)\s*$""".r.findFirstMatchIn(expr)
        .flatMap(found => {
          val path = found.group(1)
          val isFuncCall = !found.group(2).equals("")
          Option(caretPsi.getContainingFile)
            .flatMap(f => PathStrGoToDecl.getReferencedFile(path, f)).toList
            .flatMap(file => List()
              ++ resolveCommonJsFormatDef(file)
              ++ resolveRequireJsFormatDef(file)
            ).lift(0)
            .flatMap(t => if (isFuncCall) Mt.getReturnType(t) else Some(t))
        }))
  }

  private def getArgDocExprType(func: JSFunction, para: JSParameter): List[JSType] = {
    Option(JSDocumentationUtils.findDocComment(para))
      .flatMap(cast[JSDocCommentImpl](_)).toList
      .flatMap(tag => tag.getTags)
      .filter(tag => "param".equals(tag.getName))
      .filter(tag => Option(tag.getDocCommentData)
        .exists(data => Objects.equals(para.getName, data.getText)))
      .map(tag => getDocTagComment(tag))
      .flatMap(expr => parseDocExpr(para, expr))
  }

  private def resolveArg(para: JSParameter): Option[JSType] = {
    val types = Option(para.getDeclaringFunction)
      .toList.flatMap(func => List[JSType]()
        ++ getArgDocExprType(func, para)
        ++ getInlineFuncArgType(func)
        ++ getKlesunRequiresArgType(func))
    Mt.mergeTypes(types)
  }

  private def first[T](suppliers: (() => Option[T])*): Option[T] = {
    suppliers.iterator.flatMap(s => s())
      .take(1).toList.lift(0)
  }

  private def findUsages(ref: JSReferenceExpression): List[JSReferenceExpression] = {
    Option(ref.resolve()).toList.flatMap(decl => {
      val scope: PsiElement = Lang.findParent[JSFunctionExpression](decl)
        .getOrElse(decl.getContainingFile)
      Lang.findChildren[JSReferenceExpression](scope)
          .filter(usage => Objects.equals(usage.getReferenceName, ref.getReferenceName))
          .filter(usage => !Objects.equals(usage, ref))
          .filter(usage => Objects.equals(decl, usage.resolve()))
    })
  }

  private def resolveFromUsage(usage: JSReferenceExpression): GenTraversableOnce[JSType] = {
    Option(usage.getParent).toList
      .flatMap {
        // someVar.push(value)
        case superRef: JSReferenceExpression => Option(superRef.getReferenceName)
          .filter(refName => List("push", "unshift").contains(refName))
          .flatMap(refName => Option(superRef.getParent))
          .flatMap(cast[JSCallExpression](_))
          .flatMap(call => call.getArguments.lift(0))
          .flatMap(value => ctx.findExprType(value))
          .map(elT => new JSArrayTypeImpl(elT, JSTypeSource.EMPTY))
        // var someVar = null;
        // ... code
        // someVar = initializeSomething()
        case usage: JSDefinitionExpression => Option(usage.getParent)
          .flatMap(cast[JSAssignmentExpression](_))
          .flatMap(defi => Option(defi.getROperand))
          .flatMap(expr => ctx.findExprType(expr))
        case _ => List[JSType]()
      }
  }

  // may be defined in a different file unlike resolveFromUsage()
  private def resolveFromMainDecl(psi: PsiElement): GenTraversableOnce[JSType] = {
    psi match {
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

    val fromUsages = findUsages(ref)
      .flatMap(usage => resolveFromUsage(usage))

    val fromMainDecl = Option(ref.resolve()).toList
      .flatMap(psi => resolveFromMainDecl(psi))

    Mt.mergeTypes(deepRef ++ fromUsages ++ fromMainDecl)
  }
}
