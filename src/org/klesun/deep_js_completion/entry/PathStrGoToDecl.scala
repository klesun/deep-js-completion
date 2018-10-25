package org.klesun.deep_js_completion.entry

import com.intellij.codeInsight.navigation.actions.GotoDeclarationHandler
import com.intellij.lang.javascript.psi.{JSExpression, JSLiteralExpression}
import com.intellij.lang.javascript.psi.impl.JSLiteralExpressionImpl
import com.intellij.openapi.actionSystem.DataContext
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.vfs.LocalFileSystem
import com.intellij.psi.{PsiElement, PsiFile, PsiManager}
import org.klesun.lang.Lang._

import scala.collection.JavaConverters._
import PathStrGoToDecl._

object PathStrGoToDecl {
  def getReferencedFile(relPath: String, caretFile: PsiFile): Option[PsiFile] = {
    Option(caretFile.getContainingDirectory)
      .flatMap(f => Option(f.getVirtualFile))
      .map(f => f.getPath + "/" + relPath + (if (relPath.matches(".*\\.[a-zA-Z0-9]+$")) "" else ".js"))
      .flatMap(fullPath => Option(LocalFileSystem.getInstance.findFileByPath(fullPath)))
      .flatMap(f => Option(PsiManager.getInstance(caretFile.getProject).findFile(f)))
  }

  def getReferencedFile(expr: JSExpression): Option[PsiFile] = {
    cast[JSLiteralExpressionImpl](expr)
      .flatMap(lit => {
        val relPath = Option(lit.getValue).map(_.toString).getOrElse("")
        if (relPath.startsWith("./") || relPath.startsWith("../")) {
          Option(lit.getContainingFile)
            .flatMap(f => getReferencedFile(relPath, f))
        } else {
          None
        }
      })
  }
}

class PathStrGoToDecl extends GotoDeclarationHandler {
  /** @param caretPsi nullable */
  override def getGotoDeclarationTargets(caretPsi: PsiElement, mouseOffset: Int, editor: Editor): Array[PsiElement] = {
    Option(caretPsi)
      .flatMap(psi => Option(psi.getParent))
      .flatMap(cast[JSLiteralExpressionImpl](_))
      .flatMap(lit => getReferencedFile(lit)).toArray
  }

  // renames _Navigate -> GoTo_ if you make it return not null
  override def getActionText(dataContext: DataContext): String = null
}
