package org.klesun.deep_js_completion.entry

import com.intellij.codeInsight.navigation.actions.GotoDeclarationHandler
import com.intellij.lang.javascript.psi.JSExpression
import com.intellij.lang.javascript.psi.impl.JSLiteralExpressionImpl
import com.intellij.openapi.actionSystem.DataContext
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.LocalFileSystem
import com.intellij.psi.search.{FilenameIndex, ProjectScope}
import com.intellij.psi.{PsiElement, PsiFile, PsiManager}
import org.klesun.deep_js_completion.entry.PathStrGoToDecl._
import org.klesun.lang.DeepJsLang._

import scala.collection.GenTraversableOnce

object PathStrGoToDecl {
  def getReferencedFileStrict(relPath: String, caretFile: PsiFile): Option[PsiFile] = {
    Option(caretFile.getOriginalFile)
      .flatMap(f => Option(f.getContainingDirectory))
      .flatMap(f => Option(f.getVirtualFile))
      .map(f => f.getPath + "/" + relPath + (if (relPath.matches(".*\\.[a-zA-Z0-9]+$")) "" else ".js"))
      .flatMap(fullPath => {
        try {
          Option(LocalFileSystem.getInstance.findFileByPath(fullPath))
        } catch {
          case exc: Throwable => None
        }
      })
      .flatMap(f => Option(PsiManager.getInstance(caretFile.getProject).findFile(f)))
  }

  def getReferencedFileLoose(relPath: String, caretFile: PsiFile): GenTraversableOnce[PsiFile] = {
    if (relPath.startsWith("./") || relPath.startsWith("../")) {
      getReferencedFileStrict(relPath, caretFile)
    } else if (relPath.endsWith(".es6")
            || relPath.endsWith(".js")
            || relPath.endsWith(".ts")
    ) {
      // get any file in project named this way
      val proj = caretFile.getProject
      val scope = ProjectScope.getProjectScope(proj)
      FilenameIndex.getFilesByName(proj, relPath, scope)
    } else {
      None
    }
  }

  def getReferencedFileLoose(expr: JSExpression): GenTraversableOnce[PsiFile] = {
    cast[JSLiteralExpressionImpl](expr).itr()
      .flatMap(lit => {
        val relPath = Option(lit.getValue).map(_.toString).getOrElse("")
        nit(lit.getContainingFile)
          .flatMap(f => getReferencedFileLoose(relPath, f))
      })
  }
}

class PathStrGoToDecl extends GotoDeclarationHandler {
  /** @param caretPsi nullable */
  override def getGotoDeclarationTargets(caretPsi: PsiElement, mouseOffset: Int, editor: Editor): Array[PsiElement] = {
    Option(caretPsi)
      .flatMap(psi => Option(psi.getParent))
      .flatMap(cast[JSLiteralExpressionImpl](_)).itr
      .flatMap(lit => getReferencedFileLoose(lit)).toArray
  }

  // renames _Navigate -> GoTo_ if you make it return not null
  override def getActionText(dataContext: DataContext): String = null
}
