package org.klesun.deep_js_completion.completion_providers

import com.intellij.codeInsight.completion.{CompletionParameters, CompletionProvider, CompletionResultSet}
import com.intellij.codeInsight.lookup.LookupElementBuilder
import com.intellij.lang.javascript.psi.JSLiteralExpression
import com.intellij.openapi.vfs.LocalFileSystem
import com.intellij.psi.PsiWhiteSpace
import com.intellij.util.ProcessingContext
import org.klesun.lang.DeepJsLang._
import org.klesun.lang.ExplicitNull

/** provides path completion in __dirname + '/'; */
class DirnameConcatPvdr extends CompletionProvider[CompletionParameters] {
	@throws(classOf[ExplicitNull])
	def getOptions(
		parameters: CompletionParameters,
	): It[LookupElementBuilder] = {
		val pos = parameters.getEditor.getCaretModel.getOffset
		val lit = Option(parameters.getOriginalPosition)
			.flatMap(leaf => Option(leaf.getParent))
			.flatMap(cast[JSLiteralExpression](_))
			.filter(lit => {
				val prevs = getPrevSiblings(lit).itr()
					.filter(s => !s.isInstanceOf[PsiWhiteSpace]).toList
				val isConcat = prevs.exists(op => op.getText.equals("+"))
				val isDirname = prevs.exists(left => left.getText.equals("__dirname"))
				isConcat && isDirname
			})
			.unl()
		val basePath = Option(lit.getContainingFile)
			.flatMap(f => Option(f.getVirtualFile))
			.flatMap(vf => Option(vf.getCanonicalPath))
			.unl().replaceAll("\\/[^\\/]*$", "")
		val relPath = substr(lit.getStringValue, 0, pos - lit.getTextOffset - 1)
			.replaceAll("\\/[^\\/]*$", "/")
		val caretPath = basePath + relPath
		val vf = notNull(LocalFileSystem.getInstance.findFileByPath(caretPath))
		vf.getChildren.map(f => f.getName).itr()
			.map(name => LookupElementBuilder.create(relPath + name)
				.withLookupString(relPath + name)
				.withIcon(PropNamePvdr.icon))
	}

	override def addCompletions(
		parameters: CompletionParameters,
		context: ProcessingContext,
		result: CompletionResultSet
	) = {
		try {
			getOptions(parameters).foreach(result.addElement)
		} catch {
			case nulled: ExplicitNull =>
		}
	}
}
