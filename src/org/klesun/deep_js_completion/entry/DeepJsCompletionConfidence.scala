package org.klesun.deep_js_completion.entry

import com.intellij.codeInsight.completion.CompletionConfidence
import com.intellij.psi.{PsiElement, PsiFile}
import com.intellij.util.ThreeState
import org.klesun.deep_js_completion.completion_providers.JsdocPvdr

class DeepJsCompletionConfidence extends CompletionConfidence {
  // JSAutoCompletionPopupPolicy specifically disables
  // completion auto-popup in doc comment body - have to override
  // that in order to see the require('<caret>') completion
  override def shouldSkipAutopopup(contextElement: PsiElement, psiFile: PsiFile, offset: Int): ThreeState = {
    if (JsdocPvdr.matchFileNameTaker(contextElement.getText).nonEmpty) {
      ThreeState.NO
    } else {
      ThreeState.UNSURE
    }
  }
}
