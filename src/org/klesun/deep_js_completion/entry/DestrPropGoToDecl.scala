package org.klesun.deep_js_completion.entry

import com.intellij.codeInsight.navigation.actions.GotoDeclarationHandler
import com.intellij.lang.javascript.psi.types.{JSStringLiteralTypeImpl, JSTypeSource}
import com.intellij.lang.javascript.psi.{JSDestructuringElement, JSDestructuringObject, JSDestructuringShorthandedProperty, JSVariable}
import com.intellij.openapi.actionSystem.DataContext
import com.intellij.openapi.editor.Editor
import com.intellij.psi.PsiElement
import org.klesun.deep_js_completion.contexts.{ExprCtx, FuncCtx, SearchCtx}
import org.klesun.lang.DeepJsLang._


class DestrPropGoToDecl extends GotoDeclarationHandler {
	/** @param caretPsi nullable */
	override def getGotoDeclarationTargets(caretPsi: PsiElement, mouseOffset: Int, editor: Editor): Array[PsiElement] = {
		val search = new SearchCtx(40, project = Option(editor.getProject))
		val funcCtx = FuncCtx(search)
		val exprCtx = ExprCtx(funcCtx, caretPsi, 0)
		Option(caretPsi)
			.flatMap(psi => Option(psi.getParent))
			.flatMap(cast[JSVariable](_)).itr
			.flatMap(varPsi => {
				Option(varPsi.getParent)
					.flatMap(cast[JSDestructuringShorthandedProperty](_))
					.flatMap(psi => Option(psi.getParent))
					.flatMap(cast[JSDestructuringObject](_))
					.flatMap(psi => Option(psi.getParent))
					.flatMap(cast[JSDestructuringElement](_))
					.flatMap(psi => Option(psi.getInitializer)).itr
					.flatMap(qual => exprCtx.findExprType(qual))
					.flatMap(qualT => {
						val keyTOpt = Option(varPsi.getName)
							.map(name => new JSStringLiteralTypeImpl(name, true, JSTypeSource.EMPTY))
						exprCtx.mt().getKey(qualT, keyTOpt)
					})
			})
			.flatMap(t => search.typeToDecl.get(t))
			.toArray
	}

	// renames _Navigate -> GoTo_ if you make it return not null
	override def getActionText(dataContext: DataContext): String = null
}
