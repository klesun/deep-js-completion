package org.klesun.deep_js_completion.resolvers.var_res

import java.util
import java.util.Objects

import com.intellij.lang.javascript.JavascriptLanguage
import com.intellij.lang.javascript.documentation.JSDocumentationUtils
import com.intellij.lang.javascript.psi._
import com.intellij.lang.javascript.psi.ecma6.impl.TypeScriptFunctionSignatureImpl
import com.intellij.lang.javascript.psi.impl.{JSExpressionStatementImpl, JSReferenceExpressionImpl}
import com.intellij.lang.javascript.psi.jsdoc.JSDocTag
import com.intellij.lang.javascript.psi.jsdoc.impl.JSDocCommentImpl
import com.intellij.lang.javascript.psi.types._
import com.intellij.psi.impl.source.tree.LeafPsiElement
import com.intellij.psi.{PsiElement, PsiFileFactory, PsiWhiteSpace}
import org.klesun.deep_js_completion.contexts.{IExprCtx, IFuncCtx}
import org.klesun.deep_js_completion.helpers.Mt
import org.klesun.deep_js_completion.resolvers.var_res.ArgRes._
import org.klesun.deep_js_completion.resolvers.{MainRes, VarRes}
import org.klesun.deep_js_completion.structures.{JSDeepFunctionTypeImpl, JSDeepMultiType}
import org.klesun.lang.DeepJsLang
import org.klesun.lang.DeepJsLang.{cast, nit, _}

import scala.collection.GenTraversableOnce
import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

object ArgRes {

	private def grabClosureCtxs(leafCtx: IFuncCtx): GenTraversableOnce[IFuncCtx] = {
		var next: Option[IFuncCtx] = Some(leafCtx)
		var result: GenTraversableOnce[IFuncCtx] = List()
		while (next.isDefined) {
			result = next ++ result
			next = next.get.getClosureCtx()
		}
		result
	}

	private def resolveTsFuncArgArg(objt: MemIt[JSType], tsFuncDecl: TypeScriptFunctionSignatureImpl, ctx: IExprCtx, par: JSParameter, argOrder: Int): GenTraversableOnce[JSType] = {
		new GenericRes(ctx).resolveFuncArg(objt, ctx, par, tsFuncDecl).itr
			.flatMap(cast[JSFunctionTypeImpl](_))
			.flatMap(funct => funct.getParameters.asScala.lift(argOrder))
			.flatMap(arg => Option(arg.getType))
	}

	private def findReferencedFunc(funcVarDecl: PsiElement): Option[JSFunction] = {
		// TODO: add circular references check... and implementation
		funcVarDecl match {
			case varDef: JSDefinitionExpression =>
				Option(varDef.getParent)
					.flatMap(cast[JSAssignmentExpression](_))
					.flatMap(defi => Option(defi.getROperand))
					.flatMap(cast[JSFunction](_))
			case _ =>
				None
		}
	}

	private def resolvePrivateFuncUsages(par: JSParameter, ctx: IExprCtx, argOrder: Int): It[JSType] = {
		VarRes.findVarUsages(par).itr()
			.flatMap(usage => Option(usage.getParent)
				.flatMap(cast[JSCallExpression](_))
				.filter(call => usage eq call.getMethodExpression))
			.flatMap(call => call.getArguments.lift(argOrder))
			.flatMap(value => ctx.subCtxEmpty().findExprType(value))
	}
}

case class ArgRes(ctx: IExprCtx) {

	private def getFuncParam(paramOrder: Int, ref: JSReferenceExpressionImpl) = {
		val wsFuncDefs = nit(ref.resolve())
			.flatMap(funcVarDecl => findReferencedFunc(funcVarDecl))

		val deepFuncDefs = ctx.subCtxEmpty().findExprType(ref).itr()
			.flatMap(cast[JSDeepFunctionTypeImpl](_))
			.map(ft => ft.funcPsi)

		cnc(wsFuncDefs, deepFuncDefs)
			.flatMap(callerFunc => callerFunc.getParameters.lift(paramOrder)
				.flatMap(callerArg => cast[JSParameter](callerArg))
				.map(parArg => new {
					val par = parArg;
					val func = callerFunc
				}))
	}

	/** @param func - anonymous function or var holding the function */
	private def getInlineFuncArgType(func: PsiElement, argOrder: Integer): GenTraversableOnce[JSType] = {
		nit(func.getParent)
			.flatMap(cast[JSArgumentList](_))
			.flatMap(argList => nit(argList.getArguments.indexOf(func))
				.flatMap(inlineFuncArgOrder => Option(true)
					.flatMap(ok => Option(argList.getParent))
					.flatMap(cast[JSCallExpression](_)).itr
					.flatMap(call => nit(call.getMethodExpression)
						.flatMap(cast[JSReferenceExpressionImpl](_))
						.flatMap(ref => {
							val objt = nit(ref.getQualifier)
								.flatMap(obj => ctx.findExprType(obj)).mem()
							val outerCallCtx = ctx.subCtxDirect(call)

							getFuncParam(inlineFuncArgOrder, ref).flatMap(rec => {
								val privateTit = resolvePrivateFuncUsages(rec.par, ctx.subCtxEmpty(), argOrder)
								val genTit = cast[TypeScriptFunctionSignatureImpl](rec.func).itr()
									.flatMap(tsFunc => resolveTsFuncArgArg(objt, tsFunc, outerCallCtx, rec.par, argOrder))
								cnc(privateTit, genTit)
							})
						})
					)
				)
			)
	}

	// private function completion (based on scanning current
	// file for usages and taking what is passed to the function)
	private def getPrivateFuncArgType(func: JSFunction, argOrder: Integer): GenTraversableOnce[JSType] = {
		val isNoArgDoc = !ctx.func().hasArgs() && ctx.func().isInComment()
		val resolveFromUsage = !ctx.func().areArgsKnown() || isNoArgDoc
		if (!resolveFromUsage) {
			None
		} else {
			val usages = cnc(
				nit(func.getParent)
					.flatMap(cast[JSVariable](_))
					.flatMap(vari => VarRes.findVarUsages(vari)),
				VarRes.findVarUsages(func),
			)
			usages.flatMap(usage => cnc(
				// arr.map(a => someFunc(a))
				nit(usage.getParent)
					.flatMap(cast[JSCallExpression](_))
					.filter(call => usage eq call.getMethodExpression)
					.flatMap(call => call.getArguments.lift(argOrder))
					.flatMap(value => ctx.subCtxEmpty().findExprType(value))
				,
				// arr.map(someFunc)
				getInlineFuncArgType(usage, argOrder)
			))
		}
	}

	private def getCtxArgType(func: JSFunction, para: JSParameterListElement): GenTraversableOnce[JSType] = {
		val order = func.getParameters.indexOf(para)
		grabClosureCtxs(ctx.func()).itr
			.find(_.getClosurePsi().exists(_ equals func)).itr
			.flatMap(ctx => if (para.isRest) Some(ctx.getSpreadArg) else ctx.getArg(order))
	}

	def getDocTagComment(docTag: JSDocTag) = {
		var next = docTag.getNextSibling
		val tokens = new ListBuffer[PsiElement]
		while (next != null && (
			next.isInstanceOf[LeafPsiElement] ||
				next.isInstanceOf[PsiWhiteSpace]
			)) {
			tokens.append(next)
			next = next.getNextSibling
		}
		val expr = tokens.map(t => t.getText).mkString("")
			.replaceAll("""\n\s*\* """, "\n")
			.replaceAll("""\*\/$""", "")
			.replaceAll(""".*?=\s*""", "= ")
		expr
	}

	private def findVarDecl(caretPsi: PsiElement, varName: String): GenTraversableOnce[JSType] = {
		var scope: Option[PsiElement] = None
		var stmts: GenTraversableOnce[PsiElement] = Iterator.empty
		val funcOpt = DeepJsLang.findParent[JSBlockStatement](caretPsi)
		val fileOpt = DeepJsLang.findParent[JSFile](caretPsi)
		if (funcOpt.isDefined) {
			scope = funcOpt
			stmts = funcOpt.get.getStatements.itr()
		} else if (fileOpt.isDefined) {
			scope = fileOpt
			stmts = fileOpt.get.getStatements.itr()
				.flatMap(cast[JSStatement](_))
		}
		val types = scope.itr().flatMap(b => stmts.itr()
			.flatMap(st => st match {
				case varSt: JSVarStatement =>
					varSt.getDeclarations
						.filter(own => varName.equals(own.getName))
						.map(own => own.getInitializer).itr()
						.flatMap(expr => ctx.findExprType(expr))
				case func: JSFunctionDeclaration =>
					if (varName.equals(func.getName)) {
						val rts = MainRes.getReturns(func)
							.flatMap(ret => ctx.findExprType(ret))
						val rt = JSDeepMultiType(rts.mem())
						Some(new JSFunctionTypeImpl(JSTypeSource.EMPTY, new util.ArrayList, rt))
					} else {
						None
					}
				case _ => None
			})
			.++(findVarDecl(b, varName))
		)
		types
	}

	def parseDocExpr(caretPsi: PsiElement, expr: String): GenTraversableOnce[JSType] = {
		frs(
			"""^\s*=\s*(\w+)(\([^\)]*\)|)\s*$""".r.findFirstMatchIn(expr)
				.itr()
				.flatMap(found => {
					val varName = found.group(1)
					val isFuncCall = !found.group(2).equals("")
					findVarDecl(caretPsi, varName).itr()
						.flatMap(t => if (isFuncCall) Mt.getReturnType(t, ctx.subCtxEmpty()) else Some(t))
				})
			,
			"""^\s*=\s*([\s\S]+)$""".r.findFirstMatchIn(expr)
				.itr
				.flatMap(found => {
					// making it async, because IDEA does not recognize await keyword otherwise if any
					val expr = "(async () => (" + found.group(1) + "))()"
					val psiFile = PsiFileFactory.getInstance(caretPsi.getProject)
						.createFileFromText(JavascriptLanguage.INSTANCE, expr)
					nit(psiFile.getFirstChild)
						.flatMap(cast[JSExpressionStatementImpl](_))
						.flatMap(st => Option(st.getExpression))
						.flatMap(expr => ctx.subCtxDoc(caretPsi).findExprType(expr))
						.flatMap(promiset => Mt.unwrapPromise(promiset)) // since we wrapped it in async
				})
		)
	}

	private def getArgDocExprType(func: JSFunction, para: JSParameterListElement): GenTraversableOnce[JSType] = {
		Option(JSDocumentationUtils.findDocComment(para))
			.flatMap(cast[JSDocCommentImpl](_)).itr
			.flatMap(tag => tag.getTags)
			.filter(tag => "param".equals(tag.getName))
			.filter(tag => Option(tag.getDocCommentData)
				.exists(data => Objects.equals(para.getName, data.getText)))
			.map(tag => getDocTagComment(tag))
			.flatMap(expr => parseDocExpr(para, expr))
	}

	def resolve(para: JSParameterListElement): GenTraversableOnce[JSType] = {
		val types = Option(para.getDeclaringFunction)
			.itr.flatMap(func => cnc(
			getArgDocExprType(func, para),
			getCtxArgType(func, para),
			getInlineFuncArgType(func, func.getParameters.indexOf(para)),
			getPrivateFuncArgType(func, func.getParameters.indexOf(para))
		))
		types
	}
}
