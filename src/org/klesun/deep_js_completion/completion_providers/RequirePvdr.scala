package org.klesun.deep_js_completion.completion_providers

import java.nio.file.Paths

import com.intellij.codeInsight.completion.{CompletionParameters, CompletionProvider, CompletionResultSet}
import com.intellij.codeInsight.lookup.LookupElementBuilder
import com.intellij.lang.javascript.psi.{JSArgumentList, JSCallExpression, JSLiteralExpression}
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.PsiFile
import com.intellij.util.ProcessingContext
import org.klesun.deep_js_completion.entry.PathStrGoToDecl
import org.klesun.lang.DeepJsLang._

object RequirePvdr {
  def makeRelPath(caretFile: PsiFile, requireFile: VirtualFile): Option[String] = {
    Option(caretFile.getVirtualFile).map(caretFile => {
      val caretPath = Paths.get(caretFile.getCanonicalPath).getParent
      val requirePath = Paths.get(requireFile.getCanonicalPath)
      var relPath = caretPath.relativize(requirePath).toString
      relPath = relPath.replace('\\', '/') // windows
      if (!relPath.startsWith(".")) {
        relPath = "./" + relPath
      }
      relPath
    })
  }
}

class RequirePvdr extends CompletionProvider[CompletionParameters] {
  override def addCompletions(
    parameters: CompletionParameters,
    context: ProcessingContext,
    result: CompletionResultSet
  ) = {

    // works only with lowercase, since webstorm's RequiredFileSearcher discards us apparently
    // possibly should use <referencesSearch/> instead of <completion.contributor/>
    val lookups = nit(parameters.getOriginalPosition)
      .flatMap(leaf => nit(leaf.getParent))
      .flatMap(cast[JSLiteralExpression](_))
      .flatMap(lit => nit(lit.getParent)
        .flatMap(cast[JSArgumentList](_))
        .flatMap(lst => nit(lst.getParent))
        .flatMap(cast[JSCallExpression](_))
        .filter(call => nit(call.getMethodExpression)
          .exists(m => m.getText equals "require"))
        .map(call => lit.getValue + "")
        .filter(path => path.length >= 3 && !path.startsWith("."))
        .flatMap(path => PathStrGoToDecl.getFilesByPartialName(path, lit.getProject))
        .flatMap(requireFile => nit(lit.getContainingFile)
          .flatMap(caretFile => RequirePvdr.makeRelPath(caretFile, requireFile)))
        .map(full => LookupElementBuilder.create(full.replaceAll(".*\\/", ""), full)
          .withIcon(PropNamePvdr.icon))
      )
    lookups.foreach(l => {
      result.addElement(l)
    })
  }
}
