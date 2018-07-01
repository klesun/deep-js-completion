package org.klesun.deep_js_completion.entry

import com.intellij.codeInsight.navigation.actions.GotoDeclarationHandler
import com.intellij.lang.javascript.psi.JSLiteralExpression
import com.intellij.lang.javascript.psi.impl.JSLiteralExpressionImpl
import com.intellij.openapi.actionSystem.DataContext
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.vfs.LocalFileSystem
import com.intellij.psi.{PsiElement, PsiManager}
import org.klesun.lang.Lang._

import scala.collection.JavaConverters._

class PathStrGoToDecl extends GotoDeclarationHandler {
  /** @param caretPsi nullable */
  override def getGotoDeclarationTargets(caretPsi: PsiElement, mouseOffset: Int, editor: Editor): Array[PsiElement] = {
    Option(caretPsi)
      .flatMap(psi => Option(psi.getParent))
      .flatMap(cast[JSLiteralExpressionImpl](_))
      .flatMap(lit => {
        val relPath = lit.getValue.toString
        if (relPath.startsWith("./") || relPath.startsWith("../")) {
          val fullPath = lit.getContainingFile.getContainingDirectory.getVirtualFile.getPath + "/" + relPath
          Option(LocalFileSystem.getInstance.findFileByPath(fullPath))
            .flatMap(f => Option(PsiManager.getInstance(lit.getProject).findFile(f)))
        } else {
          None
        }
      }).toArray
  }

  // renames _Navigate -> GoTo_ if you make it return not null
  override def getActionText(dataContext: DataContext): String = null
}
