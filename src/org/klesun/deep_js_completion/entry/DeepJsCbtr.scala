package org.klesun.deep_js_completion.entry

import com.intellij.codeInsight.completion._
import com.intellij.lang.javascript.psi.{JSArgumentList, JSCallExpression, JSLiteralExpression, JSReferenceExpression}
import com.intellij.lang.javascript.psi.jsdoc.JSDocComment
import com.intellij.patterns.PlatformPatterns
import com.intellij.psi.PsiElement
import com.intellij.psi.impl.source.tree.LeafPsiElement
import org.klesun.deep_js_completion.completion_providers.{JsdocPvdr, PropNamePvdr, RequirePvdr}

class DeepJsCbtr extends CompletionContributor {
  this.extend(
    CompletionType.BASIC,
    PlatformPatterns.psiElement()
        .withSuperParent(0, classOf[LeafPsiElement])
        .withSuperParent(1, classOf[JSReferenceExpression])
        ,
    new PropNamePvdr()
  )
  // @param a = require('<caret>')
  // @param a = at('<caret>')
  this.extend(
    CompletionType.BASIC,
    PlatformPatterns.psiElement()
        .withSuperParent(0, classOf[LeafPsiElement])
        .withSuperParent(1, classOf[JSDocComment])
        ,
    new JsdocPvdr()
  )
  // const php = require('php.js')
  this.extend(
    CompletionType.BASIC,
    PlatformPatterns.psiElement()
        .withSuperParent(0, classOf[LeafPsiElement])
        .withSuperParent(1, classOf[JSLiteralExpression])
        .withSuperParent(2, classOf[JSArgumentList])
        .withSuperParent(3, classOf[JSCallExpression])
        ,
    new RequirePvdr()
  )

  override def invokeAutoPopup(position: PsiElement, typeChar: Char): Boolean = {
    if (typeChar == '\'' || typeChar == '"' || typeChar == '.') {
      true
    } else {
      false
    }
  }
}
