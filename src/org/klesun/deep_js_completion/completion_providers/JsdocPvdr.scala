package org.klesun.deep_js_completion.completion_providers

import java.util

import com.intellij.codeInsight.completion.{CompletionParameters, CompletionProvider, CompletionResultSet, PrioritizedLookupElement}
import com.intellij.codeInsight.lookup.{LookupElement, LookupElementBuilder}
import com.intellij.lang.javascript.psi.JSFile
import com.intellij.openapi.util.TextRange
import com.intellij.psi.PsiFile
import com.intellij.psi.search.FilenameIndex
import com.intellij.util.ProcessingContext
import org.klesun.deep_js_completion.completion_providers.PropNamePvdr.getIcon
import org.klesun.lang.Lang._

import scala.collection.GenTraversableOnce
import scala.collection.JavaConverters._
import JsdocPvdr._

import scala.util.matching.Regex.Match

object JsdocPvdr {
  private def getJsFiles(baseFile: PsiFile): GenTraversableOnce[String] = {
    FilenameIndex.getAllFilesByExt(baseFile.getProject, "js").asScala
      .filter(vf => !Option(vf.getCanonicalPath).exists(path => path.contains("/node_modules/")))
      .map(f => f.getName)
  }

  // "require('NamePart"
  // "at('"
  def matchFileNameTaker(code: String): Option[(String, String)] = {
    """((?:require|at)\(['"])([a-zA-Z][a-zA-Z0-9_\$]*|)$""".r.findFirstMatchIn(code)
      .map(found => (found.group(1), found.group(2)))
  }
}

class JsdocPvdr extends CompletionProvider[CompletionParameters] {
  override def addCompletions(
    parameters: CompletionParameters,
    context: ProcessingContext,
    result: CompletionResultSet
  ) = {
    val prefix = parameters.getEditor.getDocument
      .getText(new TextRange(Math.max(parameters.getOffset - 100, 0), parameters.getOffset))
    val postfix = parameters.getEditor.getDocument
          .getText(new TextRange(parameters.getOffset, Math.min(parameters.getOffset + 100, parameters.getEditor.getDocument.getTextLength)))
    val ending = if (!postfix.startsWith("'") && !postfix.startsWith("\"")) "')" else ""
    val lookups = matchFileNameTaker(prefix)
      .itr().flatMap((tuple) => {
        // WebStorm splits leaf PSI-s by space
        val (leafStart, namePrefix) = tuple
        nit(parameters.getOriginalFile)
          .flatMap(f =>

            getJsFiles(f).itr().flatMap(fname => {
              List(
                // idea currently excludes options not prefixed by  the
                // "require('", but this may change in future, so return both
                LookupElementBuilder.create(fname)
                  .bold().withIcon(getIcon)
                ,
                LookupElementBuilder.create(leafStart + fname + ending)
                  .bold().withIcon(getIcon)
              )
            })
          )
      })
    lookups.foreach(l => {
      result.addElement(l)
    })
  }
}
