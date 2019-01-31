package org.klesun.deep_js_completion.contexts

import com.intellij.lang.javascript.psi.{JSCallExpression, JSExpression, JSReferenceExpression, JSType}
import com.intellij.lang.javascript.psi.resolve.{JSResolveUtil, JSTypeEvaluator}
import com.intellij.lang.javascript.psi.{JSExpression, JSType}
import com.intellij.openapi.project.Project
import org.klesun.deep_js_completion.completion_providers.PropNamePvdr
import org.klesun.deep_js_completion.helpers.Mt
import org.klesun.deep_js_completion.resolvers.MainRes
import org.klesun.lang.Lang._

import scala.collection.{GenTraversable, GenTraversableOnce}
import scala.collection.JavaConverters._

class SearchCtx(
    val maxDepth: Integer = 20,
    val debug: Boolean = false,
    val project: Option[Project],
) {

    // for performance measurement
    var expressionsResolved = 0

    // for very basic GoTo
    val typeToDecl = scala.collection.mutable.Map[JSType, JSExpression]()
    // caching - to not re-resolve same expression 100500 times, also prevents many recursion cases
    val exprToResult = scala.collection.mutable.Map[JSExpression, Option[JSType]]()

    private def getWsType(expr: JSExpression) = {
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
            Option(JSTypeEvaluator.getExpressionType(expr))
              .flatMap(res => Option(res.getType))
        }
    }

    def findExprType(expr: JSExpression): Option[JSType] = {
      val funcCtx = FuncCtx(this)
      val exprCtx = ExprCtx(funcCtx, expr, 0)
      findExprType(expr, exprCtx)
    }

    private def hasTypeInfo(resolved: Option[JSType]): Boolean = {
        resolved.toList.flatMap(t =>
            project.toList.flatMap(project =>
                PropNamePvdr.getProps(t, project))
        ).nonEmpty
    }

    private def getExprChain(ctxArg: ExprCtx): List[JSExpression] = {
        var ctx = ctxArg
        var chain: List[JSExpression] = List()
        while (ctx != null) {
            if (!chain.lastOption.contains(ctx.expr)) {
                chain = List(ctx.expr) ++ chain
            }
            ctx = ctx.parent.orNull
        }
        chain
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

    def findExprType(expr: JSExpression, exprCtx: ExprCtx): Option[JSType] = {
        val indent = "  " * exprCtx.depth + "| "
        if (debug) {
            println(indent + "resolving: " + singleLine(expr.getText, 100) + " " + expr.getClass)
        }

        expressionsResolved += 1
        if (exprCtx.depth > maxDepth) {
            None
        } else if (expressionsResolved >= 7500) {
            None
        } else if (isRecursion(exprCtx)) {
            None
        } else if (exprToResult.contains(expr)) {
            if (exprToResult(expr).isEmpty && debug) {
                //Console.println("!!! circular reference\n" + getStackTrace)
            }
            exprToResult(expr)
        } else {
            exprToResult.put(expr, None)
            val resolved = MainRes.resolveIn(expr, exprCtx)
            if (debug) {
                println(indent + "resolution: " + resolved + " ||| " + singleLine(expr.getText, 350))
            }
            val result = if (hasTypeInfo(resolved)) {
                resolved
            } else {
                val builtIn = getWsType(expr)
                if (debug) {
                    println(indent + "built-in of " + builtIn.map(t => t + " " + t.getClass))
                }
                Mt.mergeTypes(resolved ++ builtIn)
            }
            if (result.isDefined) {
                exprToResult.put(expr, result)
                typeToDecl.put(result.get, expr)
            }
            result
        }
    }
}
