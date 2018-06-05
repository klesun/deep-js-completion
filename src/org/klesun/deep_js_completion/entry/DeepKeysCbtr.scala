package org.klesun.deep_js_completion.entry

import com.intellij.codeInsight.completion.{CompletionContributor, CompletionType}
import com.intellij.lang.javascript.psi.JSReferenceExpression
import com.intellij.patterns.PlatformPatterns
import com.intellij.psi.impl.source.tree.LeafPsiElement
import org.klesun.deep_js_completion.completion_providers.DeepKeysPvdr

class DeepKeysCbtr extends CompletionContributor {
  this.extend(
    CompletionType.BASIC,
    PlatformPatterns.psiElement()
        .withSuperParent(0, classOf[LeafPsiElement])
        .withSuperParent(1, classOf[JSReferenceExpression])
        ,
    new DeepKeysPvdr()
  )
}
