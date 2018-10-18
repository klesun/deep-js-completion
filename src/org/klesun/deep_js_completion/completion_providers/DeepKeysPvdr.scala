package org.klesun.deep_js_completion.completion_providers

import java.util

import com.intellij.codeInsight.completion.{CompletionParameters, CompletionProvider, CompletionResultSet, PrioritizedLookupElement}
import com.intellij.codeInsight.lookup.{LookupElement, LookupElementBuilder}
import com.intellij.lang.javascript.psi.JSRecordType.PropertySignature
import com.intellij.lang.javascript.psi.JSType.TypeTextFormat
import com.intellij.lang.javascript.psi.ecma6.impl.TypeScriptInterfaceImpl
import com.intellij.lang.javascript.psi.resolve.JSClassResolver
import com.intellij.lang.javascript.psi.types._
import com.intellij.lang.javascript.psi.{JSRecordType, JSReferenceExpression, JSType}
import com.intellij.lang.javascript.settings.JSRootConfiguration
import com.intellij.openapi.actionSystem.DataConstants
import com.intellij.psi.PsiElement
import com.intellij.psi.search.{EverythingGlobalScope, GlobalSearchScope}
import com.intellij.util.ProcessingContext
import javax.swing.ImageIcon
import org.klesun.deep_js_completion.completion_providers.DeepKeysPvdr._
import org.klesun.deep_js_completion.helpers.SearchCtx
import org.klesun.lang.Lang._

import scala.collection.JavaConverters._
import scala.collection.mutable._

object DeepKeysPvdr {
//  val imgURL = getClass.getResource("../icons/deep_16.png")
  val imgURL = getClass.getResource("../icons/deep_16_ruby2.png")
  val icon = new ImageIcon(imgURL)

  def getIcon = icon
}

class DeepKeysPvdr extends CompletionProvider[CompletionParameters] {
  override def addCompletions(
    parameters: CompletionParameters,
    context: ProcessingContext,
    result: CompletionResultSet
  ) {
    def getMaxDepth(isAutoPopup: Boolean) = {
        if (isAutoPopup) 25 else 40
    }

    def makeLookup(prop: PropertySignature, i: Int) = {
      val typeStr = Option(prop.getType)
          .map(t => t.getTypeText(TypeTextFormat.PRESENTABLE))
          .getOrElse("?")

      val name = prop.getMemberName
      val source = prop.getMemberSource
      val sourceElement = Option(source.getSingleElement).getOrElse(name)

      val lookup = LookupElementBuilder.create(sourceElement, name)
        .bold().withIcon(getIcon)
        .withTypeText(typeStr, true)
      PrioritizedLookupElement.withPriority(lookup, 200 - i)
    }

    def getProps(typ: JSType, psi: PsiElement): List[PropertySignature] = {
      typ match {
        case objT: JSRecordType => objT.getTypeMembers.asScala
          .flatMap(cast[PropertySignature](_))
          .toList
        case arrT: JSArrayType =>
          val genT = arrT.asGenericType().asRecordType()
          genT.getTypeMembers.asScala
            .flatMap(cast[PropertySignature](_)).toList
        case arrT: JSTupleTypeImpl =>
          JSClassResolver.getInstance().findClassesByQName("Array", new EverythingGlobalScope(psi.getProject)).asScala
            .flatMap(cast[TypeScriptInterfaceImpl](_))
            .toList.flatMap(ifc => ifc.getMembers.asScala)
            .flatMap(cast[PropertySignature](_))
        case mt: JSUnionOrIntersectionType =>
          mt.getTypes.asScala.flatMap(t => getProps(t, psi)).toList
        case _ =>
          /** @debug */
          //println("Unsupported typ " + typ.getClass + " " + typ)
          List()
      }
    }

    // originalPosition gives you ";" in "smfAdapter.;"
    def findRefExpr(psi: PsiElement): Option[JSReferenceExpression] = {
      psi match {
        case ref: JSReferenceExpression => Some(ref)
        case _ => psi.getChildren.flatMap(c => findRefExpr(c)).lift(0)
      }
    }

    val psi = parameters.getOriginalPosition
    val depth = getMaxDepth(parameters.isAutoPopup)
    val search = new SearchCtx().setDepth(depth)
    val startTime = System.nanoTime
    val suggestions = Option(parameters.getOriginalPosition)
      .flatMap(pos => Option(pos.getParent))
      .flatMap(findRefExpr(_))
      .flatMap(ref => Option(ref.getQualifier))
      .flatMap(qual => search.findExprType(qual))
      .toList.flatMap(typ => getProps(typ, psi))
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
    val project = if (psi != null) psi.getProject else null
    val jsConfig = if (project != null) JSRootConfiguration.getInstance(project) else null
    val onlyTyped = if (jsConfig != null) jsConfig.isOnlyTypeBasedCompletion else false

    result.runRemainingContributors(parameters, otherSourceResult => {
      val lookup = otherSourceResult.getLookupElement
      var memName = lookup.getLookupString
      if (memName.endsWith("()")) {
        memName = substr(memName, 0, -2)
      }
      var keepBuiltIn = true
      if (nameToLookup.contains(memName)) {
        // built-in already suggests this member
        if (onlyTyped) {
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
}
