package org.klesun.deep_js_completion.resolvers

import java.util.Objects

import com.intellij.lang.ecmascript6.psi.{ES6FromClause, ES6ImportedBinding}
import com.intellij.lang.javascript.psi._
import com.intellij.lang.javascript.psi.ecma6._
import com.intellij.lang.javascript.psi.ecmal4.JSClass
import com.intellij.lang.javascript.psi.impl.JSDestructuringParameterImpl
import com.intellij.lang.javascript.psi.jsdoc.JSDocComment
import com.intellij.lang.javascript.psi.resolve.JSScopeNamesCache
import com.intellij.lang.javascript.psi.types._
import com.intellij.psi.search.GlobalSearchScope
import com.intellij.psi.search.searches.ReferencesSearch
import com.intellij.psi.stubs.StubElement
import com.intellij.psi.{PsiElement, PsiFile, PsiReference}
import org.klesun.deep_js_completion.contexts.IExprCtx
import org.klesun.deep_js_completion.entry.PathStrGoToDecl
import org.klesun.deep_js_completion.helpers.Mt
import org.klesun.deep_js_completion.resolvers.VarRes._
import org.klesun.deep_js_completion.resolvers.var_res.{ArgRes, AssRes, GenericRes}
import org.klesun.deep_js_completion.structures.JSDeepClassType
import org.klesun.lang.DeepJsLang._

import scala.collection.GenTraversableOnce
import scala.collection.JavaConverters._

object VarRes {

	def findVarAt(file: PsiElement, name: String): GenTraversableOnce[JSVariable] = {
		val els = JSScopeNamesCache.findNamedElementsInStubScope(name, file).asScala
		cast[JSElement](file).itr.flatMap(file => {
			val hap = JSScopeNamesCache.getOrCreateNamesForScope(file)
			els.flatMap(cast[JSVariable](_)) ++ hap.getValues
				.flatMap(cast[JSVariable](_))
				.flatMap(ref => Option(ref.getInitializer))
				.flatMap(v => findVarAt(v, name))
		})
	}

	// possibly there is no point in a different implementation for
	// findVarAt() since we traverse whole tree anyway there as well...
	// TODO: include vars defined in methods and anonymous functions
	def findAllVarsAt(file: PsiElement): GenTraversableOnce[JSVariable] = {
		cast[JSElement](file).itr().flatMap(file => {
			val hap = JSScopeNamesCache.getOrCreateNamesForScope(file)
			val scopeVars = hap.getValues
				.flatMap(cast[JSVariable](_)).mem()
			scopeVars.itr() ++ scopeVars.itr()
				.flatMap(ref => Option(ref.getInitializer))
				.flatMap(v => findAllVarsAt(v))
		})
	}

	def findVarUsages(decl: PsiElement): GenTraversableOnce[JSReferenceExpression] = {
		if (Option(decl.getContainingFile).forall(f => f.getName.endsWith(".d.ts")) ||
			Option(decl.getContainingFile).exists(f => f.getTextLength > 3000 * 64)
		) {
			List()
		} else {
			val scope = GlobalSearchScope.fileScope(decl.getContainingFile)
			val refs: GenTraversableOnce[PsiReference] = try {
				ReferencesSearch.search(decl, scope, false).asScala
			} catch {
				// from my past experience with ReferencesSearch, it is
				// likely to throw random exceptions and cause random hangs...
				case _ => None
			}
			refs.itr.map(ref => ref.getElement)
				.filter(usage => !Objects.equals(usage, decl))
				.flatMap(cast[JSReferenceExpression](_))
				.filter(usage => Objects.equals(decl, usage.resolve()))
		}
	}

	private def findRefUsages(ref: JSReferenceExpression): GenTraversableOnce[JSReferenceExpression] = {
		nit(ref.resolve()).flatMap(decl => findVarUsages(decl))
	}
}

/**
 * resolves variable type
 */
case class VarRes(ctx: IExprCtx) {

	/**
	 * @param elDecl - either VarStatement, Variable (declared above) or destructuring array
	 * @return - type of element of the iterated array
	 */
	def resolveForInEl(elDecl: PsiElement): Option[GenTraversableOnce[JSType]] = {
		Option(elDecl.getParent)
			.flatMap(cast[JSForInStatement](_))
			.filter(st => !Objects.equals(elDecl, st.getCollectionExpression))
			.filter(st => st.isForEach)
			.flatMap(st => Option(st.getCollectionExpression))
			.map(arrexpr => ctx.findExprType(arrexpr).itr()
				.flatMap(arrt => ctx.mt().getKey(arrt, None)))
	}

	private def resolveVarSt(varst: JSVarStatement): GenTraversableOnce[JSType] = {
		varst.getChildren.flatMap(cast[JSDocComment](_))
			.flatMap(doc => doc.getTags)
			.map(tag => ArgRes(ctx.subCtxEmpty()).getDocTagComment(tag)).itr()
			.flatMap(txt => ArgRes(ctx.subCtxEmpty()).parseDocExpr(varst, txt)) ++
			resolveForInEl(varst).itr().flatMap(a => a)
	}

	private def resolveDestructEl(el: PsiElement): GenTraversableOnce[JSType] = {
		el match {
			// let doStuff = ({a, b}) => {...};
			case para: JSDestructuringParameterImpl => ArgRes(ctx).resolve(para)
			// let {a, b} = getObj();
			case obj: JSDestructuringElement =>
				nit(obj.getInitializer)
					.flatMap(qual => ctx.findExprType(qual)) ++
					nit(obj.getParent).itr
						.flatMap(cast[JSVarStatement](_))
						.flatMap(st => resolveVarSt(st))
			case _ => None
		}
	}

	def resolveDestrProp(prop: JSDestructuringProperty, dest: JSVariable): GenTraversableOnce[JSType] = {
		val isSpread = prop.getText().startsWith("...")
		nit(prop.getParent)
			.flatMap(cast[JSDestructuringObject](_))
			.flatMap(obj => Option(obj.getParent))
			.flatMap(resolveDestructEl)
			.flatMap(qualT => {
				val keyTOpt = Option(dest.getName)
					.map(name => new JSStringLiteralTypeImpl(name, true, JSTypeSource.EMPTY))
				if (isSpread) {
					val removedKeys = Option(prop.getParent).itr()
						.flatMap(cast[JSDestructuringObject](_))
						.flatMap(dobj => dobj.getProperties)
						.filter(otherProp => !otherProp.equals(prop))
						.flatMap(otherProp => Option(otherProp.getName))
						.toList
					Mt.removeKeys(qualT, removedKeys)
				} else {
					ctx.mt().getKey(qualT, keyTOpt)
				}
			})
	}

	def resolveMainDeclVar(dest: JSVariable): GenTraversableOnce[JSType] = {
		val assValTit = nit(dest.getInitializer)
			.flatMap(expr => ctx.findExprType(expr))
		val posTit = nit(dest.getParent).itr.flatMap {
			case prop: JSDestructuringProperty => resolveDestrProp(prop, dest)
			case arr: JSDestructuringArray =>
				val types = Option(arr.getParent).itr
					.flatMap(el => resolveDestructEl(el))
					.flatMap(qualT => {
						val keyTOpt = Option(arr.getElements.indexOf(dest))
							.filter(idx => idx > -1)
							.map(idx => new JSStringLiteralTypeImpl(idx + "", true, JSTypeSource.EMPTY))
						ctx.mt().getKey(qualT, keyTOpt)
					})
				types
			case varst: JSVarStatement => resolveVarSt(varst)
			case _ => None
		}
		cnc(posTit, assValTit)
	}

	// may be defined in a different file unlike resolveAssignment()
	def resolveFromMainDecl(psi: PsiElement, qualMem: MemIt[JSType]): GenTraversableOnce[JSType] = {
		psi match {
			case para: JSParameter => ArgRes(ctx).resolve(para) ++ resolveMainDeclVar(para)
			case dest: JSVariable => resolveMainDeclVar(dest)
			case prop: JSProperty => nit(prop.getValue)
				.flatMap(expr => ctx.findExprType(expr))
			case prop: JSDefinitionExpression => nit(prop.getExpression)
				.flatMap(expr => ctx.findExprType(expr))
			case tsFunc: TypeScriptFunctionSignature => {
				if (FuncRes.shouldTypedefBeIgnored(tsFunc)) {
					None
				} else {
					// possibly won't be needed, since resolved in Mt.getFlatMems()
					GenericRes(ctx).resolveFunc(tsFunc, qualMem)
				}
			}
			case func: JSFunction => FuncRes(ctx).resolve(func)
			case cls: JSClass =>
				cast[JSClass](cls)
					.map(cls => {
						val clst = JSDeepClassType(cls, ctx.subCtxEmpty())
						clst
					})
			case es6Imp: ES6ImportedBinding =>
				val moduleValue = Option(es6Imp.getDeclaration).itr()
					.flatMap(decl => findChildren[ES6FromClause](decl))
					.flatMap(cl => cl.resolveReferencedElements().asScala)
					.flatMap(cast[PsiFile](_))
					.flatMap(ModuleRes(ctx.subCtxEmpty()).resolveWebpackImport)
				if (es6Imp.getText.startsWith("* as ")) {
					moduleValue
				} else {
					val keyt = new JSStringLiteralTypeImpl("default", true, JSTypeSource.EMPTY)
					moduleValue.flatMap(t => ctx.mt().getKey(t, Some(keyt)))
				}
			case _ =>
				//println("Unsupported var declaration - " + psi.getClass + " " + psi.getText)
				None
		}
	}

	private def getDeclarationsFromWs(ref: JSReferenceExpression): GenTraversableOnce[PsiElement] = {
		val isProp = ref.getQualifier != null
		// it would be nice to always use es2018 instead of es2015 somehow
		val psis = Option(ref.resolve()).itr
			.filter(decl => {
				val isDts = Option(decl.getContainingFile).exists(f => f.getName endsWith ".d.ts")
				// skip definitions that are actually just random props in project with same name
				!isProp || isDts
			})
		psis
	}

	// an imaginary at('Module.js') function used in docs to
	// specify any var defined in the file with the following name
	private def assertAtModuleEpxr(qual: JSExpression): GenTraversableOnce[PsiFile] = {
		cast[JSCallExpression](qual)
			.filter(call => Option(call.getMethodExpression).exists(meth => meth.getText equals "at"))
			.flatMap(call => call.getArguments.lift(0)).itr
			.flatMap(PathStrGoToDecl.getReferencedFileAnyDir)
	}

	private def assertThisCtorRef(ref: JSReferenceExpression): GenTraversableOnce[JSType] = {
		if (ref.getText.equals("this.constructor")) {
			nit(ref.getQualifier)
				.flatMap(cast[JSThisExpression](_))
				.flatMap(thisPsi => MainRes.getThisCls(thisPsi))
				.flatMap(rec => {
					if (rec.isStatic) {
						None // not sure this.constructor would even be defined in a static call...
					} else {
						Some(JSDeepClassType(rec.clsPsi, ctx.subCtxEmpty()))
					}
				})
		} else {
			None
		}
	}

	def resolve(ref: JSReferenceExpression): GenTraversableOnce[JSType] = {
		val qualMem = nit(ref.getQualifier)
			.flatMap(qual => ctx.findExprType(qual)).mem()
		var tit = cnc(
			getDeclarationsFromWs(ref).itr()
				.flatMap(psi => resolveFromMainDecl(psi, qualMem))
			,
			qualMem.itr()
				.flatMap(qualT => {
					val keyTOpt = Option(ref.getReferenceName)
						.map(name => new JSStringLiteralTypeImpl(name, true, JSTypeSource.EMPTY))
					val result = ctx.mt().getKey(qualT, keyTOpt)
					result
				})
			,
			Option(ref.getReferenceName).itr()
				.flatMap(varName => Option(ref.getQualifier).itr()
					.flatMap(assertAtModuleEpxr)
					.flatMap(file => findVarAt(file, varName))
					.flatMap(vari => resolveMainDeclVar(vari)))
			,
			findRefUsages(ref).itr()
				.flatMap(usage => new AssRes(ctx)
					.resolveAssignmentTo(usage)
					.itr().flatMap(a => a))
			,
			assertThisCtorRef(ref)
		)
		tit
	}
}
