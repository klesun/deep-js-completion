package org.klesun.deep_js_completion.contexts

import com.intellij.lang.javascript.psi.resolve.JSTypeEvaluator
import com.intellij.lang.javascript.psi.types.JSTypeImpl
import com.intellij.lang.javascript.psi._
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import org.klesun.deep_js_completion.entry.DeepJsSettings
import org.klesun.deep_js_completion.helpers.Mt
import org.klesun.deep_js_completion.resolvers.MainRes
import org.klesun.deep_js_completion.structures.DeepIndexSignatureImpl
import org.klesun.lang.DeepJsLang._

import scala.collection.JavaConverters._
import scala.collection.{GenTraversableOnce, mutable}

object SearchCtx {
	def formatPsi(element: PsiElement): String = {
		val line = element.getContainingFile.getText
			.slice(0, element.getTextOffset).split("\n").length
		element.getText.replace("\n", " \\n ") + ":" + line
	}
}

class SearchCtx(
	val maxDepth: Integer = 20,
	val project: Option[Project],
) {
	// for performance measurement
	var expressionsResolved = 0

	// for very basic GoTo
	val typeToDecl = scala.collection.mutable.Map[JSType, JSExpression]()
	// caching - to not re-resolve same expression 100500 times, also prevents many recursion cases
	val ctxToExprToResult = mutable.Map[IFuncCtx, mutable.Map[JSExpression, MemIt[JSType]]]()

	private def getMaxExpressions(): Integer = {
		project
			.map(p => DeepJsSettings.inst(p).totalExpressionLimit)
			.getOrElse(7500)
	}

	private def getWsType(expr: JSExpression): GenTraversableOnce[JSType] = {
		val isProp = cast[JSReferenceExpression](expr)
			.exists(ref => ref.getQualifier != null)
		val isMeth = cast[JSCallExpression](expr)
			.flatMap(call => Option(call.getMethodExpression))
			.flatMap(cast[JSReferenceExpression](_))
			.exists(ref => ref.getQualifier != null)

		if (isProp || isMeth) {
			// no point resolving object member access: firstly, it searches all members with same name in project if qualifier
			// could not be resolved - that's bad, and secondly, we already resolved the object itself by the moment nevertheless
			None
		} else {
			// would be nice to find a better function - that would
			// try _guessing_ declaration by just member name
			Option(JSTypeEvaluator.getElementType(expr))
				.flatMap(res => if (res.getResults().size() > 0) Some(res.getResults().get(0)) else None)
				.flatMap(res => Option(res.getType))
		}
	}

	def findExprType(expr: JSExpression): GenTraversableOnce[JSType] = {
		val funcCtx = FuncCtx(this)
		val exprCtx = ExprCtx(funcCtx, expr, 0)
		findExprType(expr, exprCtx)
	}

	private def takeFromCache(ctx: IExprCtx, expr: JSExpression): Option[MemIt[JSType]] = {
		if (!ctxToExprToResult.contains(ctx.func())) {
			None
		} else if (!ctxToExprToResult(ctx.func()).contains(expr)) {
			None
		} else {
			Some(ctxToExprToResult(ctx.func())(expr))
		}
	}

	private def putToCache(ctx: IExprCtx, expr: JSExpression, result: MemIt[JSType]): Unit = {
		if (!ctxToExprToResult.contains(ctx.func())) {
			ctxToExprToResult.put(ctx.func(), mutable.Map())
		}
		ctxToExprToResult(ctx.func()).remove(expr)
		ctxToExprToResult(ctx.func()).put(expr, result)
	}

	private def getExprChain(ctxArg: ExprCtx): List[PsiElement] = {
		var ctx = ctxArg
		var chain: List[PsiElement] = List()
		while (ctx != null) {
			if (!chain.lastOption.contains(ctx.expr)) {
				chain = chain ++ List(ctx.expr)
			}
			ctx = ctx.parent.orNull
		}
		chain.reverse
	}

	private def endsWith[T](superList: List[T], subList: List[T]): Boolean = {
		var endsWith = true
		var i = 0
		while (i < subList.length && endsWith) {
			if (i >= superList.length) {
				endsWith = false
			} else {
				val left = superList(superList.size - i - 1)
				val right = subList(subList.size - i - 1)
				if (!left.equals(right)) {
					endsWith = false
				}
			}
			i += 1
		}
		endsWith
	}

	/** @untested, possibly has mistakes */
	private def isRecursion(ctx: ExprCtx) = {
		// imagine sequence: a b c d e f g e f g
		//                           ^_____^_____
		// from my experience this assumption is right -  I
		// treat any case where end repeats pre-end as recursion
		var isRecursion = false
		val psiTrace = getExprChain(ctx)
		var i = 0
		while (i < psiTrace.length / 2 && !isRecursion) {
			val start = psiTrace.length - i * 2 - 2
			val subList = psiTrace.slice(start, start + i + 1)
			if (endsWith(psiTrace, subList)) {
				isRecursion = true
			}
			i += 1
		}
		isRecursion
	}

	def formatType(jst: JSType): String = {
		jst match {
			case rect: JSRecordType => "{\n" + rect.getTypeMembers.asScala.map {
				case deep: DeepIndexSignatureImpl => {
					deep.psi + ""
				}
				case mem => "    mem: " + (mem.getClass + "").replaceAll(".*\\.", "") + " - " + mem
			}.mkString(",\n") + "\n}"
			case _ => jst + " of " + (jst.getClass + "")
		}
	}

	case class FqnTypeHash(fqnt: JSTypeImpl) {
		private val fqn = fqnt.getTypeText()

		override def hashCode(): Int = fqn.hashCode()

		override def equals(that: Any): Boolean = {
			cast[FqnTypeHash](that).exists(that => {
				// this operation seems to be CPU consuming, I thought of extending
				// JSTypeImpl to make the FQN string public, but the class is final sadly...
				val otherFqn = that.fqnt.getTypeText()
				fqn.equals(otherFqn)
			})
		}
	}

	private def makeTypeHash(jst: JSType) = {
		jst match {
			case null => null
			case fqnt: JSTypeImpl => FqnTypeHash(fqnt)
			case _ => jst
		}
	}

	def findExprType(expr: JSExpression, exprCtx: ExprCtx): GenTraversableOnce[JSType] = {
		val indent = "  " * exprCtx.depth + "| "
		val startNs = System.nanoTime
		if (Debug.PRINT_RESOLVE_START) {
			println(indent + "resolving: " + singleLine(expr.getText, 100) + " " + expr.getClass)
		}

		expressionsResolved += 1
		// I dunno why key resolution affects the completion
		// without this check, possibly some iterator is reused twice...
		val fromCache = if (exprCtx.doNotCache) None else takeFromCache(exprCtx, expr)
		if (fromCache.nonEmpty) {
			fromCache.get
		} else if (exprCtx.depth > maxDepth) {
			None
		} else if (expressionsResolved >= getMaxExpressions()) {
			None
		} else if (isRecursion(exprCtx)) {
			None
		} else {
			if (!exprCtx.doNotCache) {
				putToCache(exprCtx, expr, Iterator.empty.mem())
			}
			val resolved = MainRes.resolveIn(expr, exprCtx).itr
			// no point getting built-in type here, IDEA will show it itself
			val isAtCaret = exprCtx.parent.isEmpty
			val builtIn = getWsType(expr).filter(t => !isAtCaret)
			var result = frs(resolved, builtIn)
			val mit = result.flatMap(t => Mt.flattenTypes(t)).unq(makeTypeHash).mem()

			val postfix = " ||| " + singleLine(expr.getText, 350)
			// TODO: one of types happens to be null sometimes - fix!
			if (Debug.PRINT_RESOLVE_RESULT_FIRST) {
				val durationSec = (System.nanoTime() - startNs) / 1000000000.0
				var prefix = indent + "resolution: "
				if (durationSec > 0.001) {
					prefix += "in " + (Math.round(durationSec * 1000) / 1000.0) + " "
				}
				println(prefix + mit.fst().map(a => "fst: " + a + " " + a.getClass) + postfix)
			}
			if (Debug.PRINT_RESOLVE_RESULT_FULL) {
				println(indent + "resolution: " + mit.map(a => a + " " + a.getClass).toList + postfix)
			}

			if (!exprCtx.doNotCache) {
				putToCache(exprCtx, expr, mit)
			}
			mit.itr().map(t => {
				typeToDecl.put(t, expr)
				t
			})
		}
	}
}
