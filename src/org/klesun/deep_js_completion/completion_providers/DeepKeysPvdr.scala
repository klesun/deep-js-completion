package org.klesun.deep_js_completion.completion_providers

import com.intellij.codeInsight.completion.{CompletionParameters, CompletionProvider, CompletionResultSet}
import com.intellij.codeInsight.lookup.LookupElementBuilder
import com.intellij.lang.javascript.psi.JSReferenceExpression
import com.intellij.util.{PlatformIcons, ProcessingContext}
import icons.JavaScriptPsiIcons

class DeepKeysPvdr extends CompletionProvider[CompletionParameters] {
  override def addCompletions(
    parameters: CompletionParameters,
    context: ProcessingContext,
    result: CompletionResultSet
  ) {
    def toCast[T] (obj: Object): Option[T] = obj match {
      case matching: T => Some(matching)
      case _ => None
    }

    def log(msg: String) = {
      println(msg)
      true
    }

    val psi = parameters.getPosition

    val suggestion = Option(parameters.getPosition.getParent)
      .flatMap(toCast[JSReferenceExpression])
      .flatMap(ref => Option(ref.getQualifier))
      .map(qual => qual.getText)
      .getOrElse(() => "wrong psi")

    result.addElement(LookupElementBuilder.create("test " + suggestion)
      .bold()
      .withIcon(PlatformIcons.PROPERTY_ICON)
      .withTailText(" = doSomething()")
      .withTypeText("string"))
  }
}
