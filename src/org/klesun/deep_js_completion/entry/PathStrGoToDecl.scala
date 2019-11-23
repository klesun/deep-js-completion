package org.klesun.deep_js_completion.entry

import com.intellij.codeInsight.completion.impl.CamelHumpMatcher
import com.intellij.codeInsight.navigation.actions.GotoDeclarationHandler
import com.intellij.json.psi.{JsonFile, JsonObject, JsonStringLiteral}
import com.intellij.lang.javascript.library.JSLibraryManager
import com.intellij.lang.javascript.psi.JSExpression
import com.intellij.lang.javascript.psi.impl.JSLiteralExpressionImpl
import com.intellij.openapi.actionSystem.DataContext
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.{LocalFileSystem, VirtualFile}
import com.intellij.psi.search.{FilenameIndex, ProjectScope}
import com.intellij.psi.{PsiElement, PsiFile, PsiManager}
import org.klesun.deep_js_completion.entry.PathStrGoToDecl._
import org.klesun.lang.DeepJsLang._

import scala.collection.GenTraversableOnce
import scala.collection.JavaConverters._

object PathStrGoToDecl {
  def getByFullPath(fullPathArg: String, proj: Project): Option[PsiFile] = {
    val fullPath = fullPathArg + (if (fullPathArg.matches(".*\\.[a-zA-Z0-9]+$")) "" else ".js")
    try {
      Option(LocalFileSystem.getInstance.findFileByPath(fullPath))
        .flatMap(f => Option(PsiManager.getInstance(proj).findFile(f)))
    } catch {
      case exc: Throwable => None
    }
  }

  def getRelativeFile(relPath: String, caretFile: PsiFile): Option[PsiFile] = {
    Option(caretFile.getOriginalFile)
      .flatMap(f => Option(f.getContainingDirectory))
      .flatMap(f => Option(f.getVirtualFile))
      .map(f => f.getPath + "/" + relPath)
      .flatMap(fullPath => getByFullPath(fullPath, caretFile.getProject))
  }

  def getReferencedFileAnyDir(relPath: String, caretFile: PsiFile): GenTraversableOnce[PsiFile] = {
    if (relPath.startsWith("./") || relPath.startsWith("../")) {
      getRelativeFile(relPath, caretFile)
    } else if (relPath.endsWith(".es6")
            || relPath.endsWith(".js")
            || relPath.endsWith(".ts")
            || relPath.endsWith(".json")
            || relPath.endsWith(".json5")
    ) {
      // get any file in project named this way
      val proj = caretFile.getProject
      val scope = ProjectScope.getProjectScope(proj)
      FilenameIndex.getFilesByName(proj, relPath, scope)
    } else {
      None
    }
  }

  def getFilesByPartialName(partialName: String, proj: Project): GenTraversableOnce[VirtualFile] = {
    val scope = ProjectScope.getProjectScope(proj)
    val matcher = new CamelHumpMatcher(partialName, false)
    FilenameIndex.getAllFilesByExt(proj, "js", scope).asScala
      .filter(f => matcher.prefixMatches(f.getName))
      .filter(vf => !Option(vf.getCanonicalPath).exists(path => path.contains("/node_modules/")))
  }

  def getReferencedFileAnyDir(expr: JSExpression): GenTraversableOnce[PsiFile] = {
    cast[JSLiteralExpressionImpl](expr).itr()
      .flatMap(lit => {
        val relPath = Option(lit.getValue).map(_.toString).getOrElse("")
        nit(lit.getContainingFile)
          .flatMap(f => getReferencedFileAnyDir(relPath, f))
      })
  }

  def getModuleFile(expr: JSExpression): GenTraversableOnce[PsiFile] = {
    cast[JSLiteralExpressionImpl](expr).itr()
      .flatMap(lit => {
        var moduleName = Option(lit.getValue).map(_.toString).getOrElse("")
        val projRoot = Option(expr.getProject.getBasePath).getOrElse("")
        val libRootRelPaths = JSLibraryManager.getInstance(expr.getProject)
          .getLibraryMappings.getAvailableValues.asScala
          .map(libModel => libModel.getName.replaceAll("^[^\\/]+\\/", ""))
          .toList.++(List("node_modules"))

        libRootRelPaths.flatMap(libRootRelPath => {
            val libRoot = projRoot + "/" + libRootRelPath
            val modulePath = libRoot + "/" + moduleName
            if (moduleName.contains("/")) {
              // some specific file inside the lib is referenced
              getByFullPath(modulePath, expr.getProject)
            } else {
              // main library file is referenced (specified in package.json)
              val packJsonPath = modulePath + "/package.json"
              getByFullPath(packJsonPath, expr.getProject)
                .flatMap(cast[JsonFile](_))
                .flatMap(json => Option(json.getTopLevelValue))
                .flatMap(cast[JsonObject](_))
                .flatMap(jsObj => Option(jsObj.findProperty("main")))
                .flatMap(prop => Option(prop.getValue))
                .flatMap(cast[JsonStringLiteral](_))
                .map(lit => modulePath + "/" + lit.getValue)
                .flatMap(fullPath => getByFullPath(fullPath, expr.getProject))
            }
          })
      })
  }
}

class PathStrGoToDecl extends GotoDeclarationHandler {
  /** @param caretPsi nullable */
  override def getGotoDeclarationTargets(caretPsi: PsiElement, mouseOffset: Int, editor: Editor): Array[PsiElement] = {
    Option(caretPsi)
      .flatMap(psi => Option(psi.getParent))
      .flatMap(cast[JSLiteralExpressionImpl](_)).itr
      .flatMap(lit => getReferencedFileAnyDir(lit)).toArray
  }

  // renames _Navigate -> GoTo_ if you make it return not null
  override def getActionText(dataContext: DataContext): String = null
}
