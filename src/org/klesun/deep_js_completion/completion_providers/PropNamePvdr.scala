package org.klesun.deep_js_completion.completion_providers

import java.util

import com.intellij.codeInsight.completion.{CompletionParameters, CompletionProvider, CompletionResultSet, PrioritizedLookupElement}
import com.intellij.codeInsight.lookup.{LookupElement, LookupElementBuilder}
import com.intellij.codeInsight.navigation.actions.GotoDeclarationHandler
import com.intellij.lang.javascript.psi.JSRecordType.PropertySignature
import com.intellij.lang.javascript.psi.JSType.TypeTextFormat
import com.intellij.lang.javascript.psi.ecma6.impl.TypeScriptInterfaceImpl
import com.intellij.lang.javascript.psi.resolve.JSClassResolver
import com.intellij.lang.javascript.psi.types._
import com.intellij.lang.javascript.psi.{JSRecordType, JSReferenceExpression, JSType}
import com.intellij.lang.javascript.settings.JSRootConfiguration
import com.intellij.openapi.actionSystem.{DataConstants, DataContext}
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import com.intellij.psi.search.{EverythingGlobalScope, GlobalSearchScope}
import com.intellij.util.ProcessingContext
import javax.swing.ImageIcon
import org.klesun.deep_js_completion.completion_providers.PropNamePvdr._
import org.klesun.deep_js_completion.contexts.SearchCtx
import org.klesun.lang.Lang._

import scala.collection.JavaConverters._
import scala.collection.mutable._
import org.klesun.deep_js_completion.completion_providers._

import scala.collection.GenTraversableOnce

object PropNamePvdr {
//  val imgURL = getClass.getResource("../icons/deep_16.png")
  val imgURL = getClass.getResource("../icons/deep_16_ruby2.png")
  val icon = new ImageIcon(imgURL)

  def getIcon = icon

  private def getMaxDepth(isAutoPopup: Boolean) = {
    if (isAutoPopup) 25 else 40
  }

  private def makeLookup(prop: PropertySignature, i: Int) = {
    val typeStr = Option(prop.getType)
      .map {
        case lit: JSPrimitiveLiteralType[Any] =>
          val strVal = lit.getLiteral + ""
          if (strVal.trim equals "") lit.getTypeText(TypeTextFormat.PRESENTABLE) else "'" + strVal + "'"
        case t => t.getTypeText(TypeTextFormat.PRESENTABLE)
      }
      .getOrElse("?")

    val name = prop.getMemberName
    val source = prop.getMemberSource
    val sourceElement = Option(source.getSingleElement).getOrElse(name)

    val lookup = LookupElementBuilder.create(sourceElement, name)
      .bold().withIcon(getIcon)
      .withTypeText(typeStr, true)
    PrioritizedLookupElement.withPriority(lookup, 200 - i)
  }

  private def getProps(typ: JSType, project: Project): List[PropertySignature] = {
    typ match {
      case objT: JSRecordType => objT.getTypeMembers.asScala
        .flatMap(cast[PropertySignature](_))
        .toList
      case arrT: JSArrayType =>
        // JSTypeBaseImpl should already cover that
        val genT = arrT.asGenericType().asRecordType()
        genT.getTypeMembers.asScala
          .flatMap(cast[PropertySignature](_)).toList
      case arrT: JSTupleTypeImpl =>
        JSClassResolver.getInstance().findClassesByQName("Array", new EverythingGlobalScope(project)).asScala
          .flatMap(cast[TypeScriptInterfaceImpl](_))
          .toList.flatMap(ifc => ifc.getMembers.asScala)
          .flatMap(cast[PropertySignature](_))
      case mt: JSUnionOrIntersectionType =>
        mt.getTypes.asScala.flatMap(t => getProps(t, project)).toList
      case mt: JSGenericTypeImpl =>
        val fqn = mt.getType.getTypeText(TypeTextFormat.CODE)
        JSClassResolver.getInstance().findClassesByQName(fqn, new EverythingGlobalScope(project)).asScala
          .toList.flatMap(ifc => ifc.getMembers.asScala)
          .flatMap(cast[PropertySignature](_))
      case mt: JSTypeBaseImpl =>
        // when you specify class with jsdoc for example - JSTypeImpl
        mt.asRecordType().getTypeMembers.asScala
          .flatMap(cast[PropertySignature](_)).toList
      case _ =>
        /** @debug */
        //println("Unsupported typ " + typ.getClass + " " + typ)
        List()
    }
  }
}

class PropNamePvdr extends CompletionProvider[CompletionParameters] with GotoDeclarationHandler {
  override def addCompletions(
    parameters: CompletionParameters,
    context: ProcessingContext,
    result: CompletionResultSet
  ) {

    // originalPosition gives you ";" in "smfAdapter.;"
    def findRefExpr(psi: PsiElement): Option[JSReferenceExpression] = {
      psi match {
        case ref: JSReferenceExpression => Some(ref)
        case _ => psi.getChildren.flatMap(c => findRefExpr(c)).lift(0)
      }
    }

    val nullPsi = parameters.getPosition
    val depth = getMaxDepth(parameters.isAutoPopup)
    val search = new SearchCtx(depth)
    val startTime = System.nanoTime
    val suggestions = Option(nullPsi)
      .flatMap(pos => Option(pos.getParent))
      .flatMap(findRefExpr(_))
      .flatMap(ref => Option(ref.getQualifier))
      .flatMap(qual => search.findExprType(qual))
      .toList.flatMap(typ => getProps(typ, nullPsi.getProject))
      .filter(prop => !prop.getMemberName.startsWith("[Symbol."))
      .zipWithIndex
      .map({case (e,i) => makeLookup(e,i)})

    val elapsed = System.nanoTime - startTime
    result.addLookupAdvertisement("Resolved in " + (elapsed / 1000000000.0) + " seconds")

    // test
    // test
    // test
    // test

    val nameToLookup = ListMap(suggestions.map(t => t.getLookupString -> t) : _*)
    val builtInSuggestions = new util.ArrayList[LookupElement]
    val project = if (nullPsi != null) nullPsi.getProject else null
    val jsConfig = if (project != null) JSRootConfiguration.getInstance(project) else null
    val onlyTyped = if (jsConfig != null) jsConfig.isOnlyTypeBasedCompletion else false

    result.runRemainingContributors(parameters, otherSourceResult => {
      val lookup = otherSourceResult.getLookupElement
      // 99.0 (group 94) - inferred type property completion
      // 5.0 (group 6) - property completion guessed from usage
      val isGuess = cast[PrioritizedLookupElement[LookupElement]](lookup)
        .forall(pri => pri.getPriority < 99.0)

      var memName = lookup.getLookupString
      if (memName.endsWith("()")) {
        memName = substr(memName, 0, -2)
      }
      var keepBuiltIn = true
      if (nameToLookup.contains(memName)) {
        // built-in already suggests this member
        if (onlyTyped && !isGuess) {
          // built-in suggestion is qualitative, keep it, remove ours
          nameToLookup.remove(memName)
        } else {
          // built-in suggestion is mixed with rubbish - remove it, keep ours
          keepBuiltIn = false
        }
      }
      if (keepBuiltIn) {
        builtInSuggestions.add(lookup)
      }
    })

    result.addAllElements(nameToLookup.values.asJava)
    result.addAllElements(builtInSuggestions)
  }

  override def getGotoDeclarationTargets(caretPsi: PsiElement, mouseOffset: Int, editor: Editor): Array[PsiElement] = {
    val depth = getMaxDepth(false)
    val search = new SearchCtx(depth)

    Option(caretPsi)
      .flatMap(leaf => Option(leaf.getParent))
      .flatMap(cast[JSReferenceExpression](_))
      .toList.flatMap(ref => Option(ref.getQualifier)
        .flatMap(qual => search.findExprType(qual))
        .toList.flatMap(typ => getProps(typ, caretPsi.getProject))
        .filter(prop => prop.getMemberName.equals(ref.getReferenceName)))
      // TODO: store PSI per key, not per value: there is a lot of rubbish now
      .flatMap(p => search.typeToDecl.get(p.getType))
      .distinct
      .toArray
  }

  def getActionText(dataContext: DataContext): String = null
}
