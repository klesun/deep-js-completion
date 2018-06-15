package org.klesun.deep_js_completion.completion_providers

import java.net.URL
import java.util

import com.intellij.codeInsight.completion.{CompletionParameters, CompletionProvider, CompletionResultSet, PrioritizedLookupElement}
import com.intellij.codeInsight.lookup.{LookupElement, LookupElementBuilder}
import com.intellij.lang.javascript.psi.{JSRecordType, JSReferenceExpression, JSType}
import com.intellij.openapi.util.IconLoader
import com.intellij.util.{PlatformIcons, ProcessingContext}
import icons.JavaScriptPsiIcons
import javax.swing.ImageIcon
import DeepKeysPvdr._
import com.intellij.lang.javascript.psi.JSRecordType.{IndexSignature, PropertySignature}
import com.intellij.lang.javascript.psi.JSType.TypeTextFormat
import com.intellij.lang.javascript.psi.resolve.JSTypeEvaluator
import com.intellij.lang.javascript.psi.types.{JSArrayType, JSContextualUnionTypeImpl, JSGenericTypeImpl, JSUnknownType}
import com.intellij.lang.javascript.psi.types.JSRecordTypeImpl.PropertySignatureImpl
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

    def getProps(typ: JSType): List[PropertySignature] = {
      typ match {
        case objT: JSRecordType => objT.getTypeMembers.asScala
          .flatMap(cast[PropertySignature](_))
          .toList
        case arrT: JSArrayType =>
          val genT = arrT.asGenericType().asRecordType()
          genT.getTypeMembers.asScala
            .flatMap(cast[PropertySignature](_)).toList
        case mt: JSContextualUnionTypeImpl =>
          mt.getTypes.asScala.flatMap(t => getProps(t)).toList
        case _ =>
          List()
      }
    }

    val psi = parameters.getPosition
    val depth = getMaxDepth(parameters.isAutoPopup)
    val search = new SearchCtx().setDepth(depth)

    val suggestions = Option(parameters.getPosition.getParent)
      .flatMap(cast[JSReferenceExpression](_))
      .flatMap(ref => Option(ref.getQualifier))
      .flatMap(qual => search.findExprType(qual))
      .toList.flatMap(typ => getProps(typ))
      .filter(prop => !prop.getMemberName.startsWith("[Symbol."))
      .zipWithIndex
      .map({case (e,i) => makeLookup(e,i)})

    /** @debug */
    println("resolved: " + suggestions.map(s => s.getLookupString))

    val nameToLookup = ListMap(suggestions.map(t => t.getLookupString -> t) : _*)
    val builtInSuggestions = new util.ArrayList[LookupElement]

    result.runRemainingContributors(parameters, otherSourceResult => {
      // remove dupe built-in suggestions
      val lookup = otherSourceResult.getLookupElement
      nameToLookup.remove(lookup.getLookupString)
      if (lookup.getLookupString.endsWith("()")) {
        nameToLookup.remove(substr(lookup.getLookupString, 0, -2))
      }
      builtInSuggestions.add(lookup)
    })

    result.addAllElements(nameToLookup.values.asJava)
    result.addAllElements(builtInSuggestions)
  }
}
