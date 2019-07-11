
package org.klesun.deep_js_completion.entry

import com.intellij.codeInsight.intention.IntentionAction
import com.intellij.lang.javascript.psi.JSReferenceExpression
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiFile
import com.intellij.psi.search.{FilenameIndex, ProjectScope}
import com.intellij.psi.util.PsiTreeUtil
import org.klesun.deep_js_completion.completion_providers.RequirePvdr

class AddRequireByVarNameInt extends IntentionAction {
  override def getFamilyName = "Module import"

  override def getText = "Add require() by var name"

  override def startInWriteAction(): Boolean = true

  private def assertUndeclaredVar(project: Project, editor: Editor, psiFile: PsiFile): Option[JSReferenceExpression] = {
    val offset = editor.getCaretModel.getOffset
    // -1 and 0 because if caret is after last char of the var name, IDEA
    // highlights it, but findElementOfClassAtOffset does not match it
    List(-1, 0).flatMap(searchShift => Option(psiFile)
      .flatMap(f =>  Option(PsiTreeUtil.findElementOfClassAtOffset(
        f, offset + searchShift, classOf[JSReferenceExpression], false
      )))
      .filter(ref => ref.getQualifier == null)
      .filter(ref => ref.resolve() == null)
    ).headOption
  }

  override def isAvailable(project: Project, editor: Editor, psiFile: PsiFile): Boolean = {
    // available if caret is put on a var, and this var is undeclared
    assertUndeclaredVar(project, editor, psiFile).nonEmpty
  }

  override def invoke(project: Project, editor: Editor, psiFile: PsiFile): Unit = {
    val invoked = assertUndeclaredVar(project, editor, psiFile)
      .flatMap(ref => {
        val varName = ref.getReferenceName
        val scope = ProjectScope.getProjectScope(project)
        FilenameIndex.getFilesByName(project, varName + ".js", scope)
          .flatMap(f => Option(f.getVirtualFile))
          .flatMap(vf => RequirePvdr.makeRelPath(psiFile, vf))
          .toList.sortBy(relPath => relPath.length)
          .headOption
          .map(relPath => {
            val reqSt = "const " + varName + " = require('" + relPath + "');\n"
            editor.getDocument.insertString(0, reqSt)
          })
      })
    if (invoked.isEmpty) {
      // TODO: show "Failed to locate file by var name" error
    }
  }
}
