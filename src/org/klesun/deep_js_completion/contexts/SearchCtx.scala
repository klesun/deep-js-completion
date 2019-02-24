package org.klesun.deep_js_completion.contexts

import com.intellij.lang.javascript.psi.resolve.JSTypeEvaluator
import com.intellij.lang.javascript.psi.{JSCallExpression, JSExpression, JSReferenceExpression, JSType}
import com.intellij.openapi.project.Project
import org.klesun.deep_js_completion.completion_providers.PropNamePvdr
import org.klesun.deep_js_completion.resolvers.MainRes
import org.klesun.lang.DeepJsLang._

import scala.collection.{GenTraversableOnce, mutable}

object SearchCtx {
  val DEBUG_DEFAULT = false
}

class SearchCtx(
    val maxDepth: Integer = 20,
    val debug: Boolean = SearchCtx.DEBUG_DEFAULT,
    val project: Option[Project],
) {
    // for performance measurement
    var expressionsResolved = 0

    // for very basic GoTo
    val typeToDecl = scala.collection.mutable.Map[JSType, JSExpression]()
    // caching - to not re-resolve same expression 100500 times, also prevents many recursion cases
    val ctxToExprToResult = mutable.Map[IFuncCtx, mutable.Map[JSExpression, MemIt[JSType]]]()

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

    def findExprType(expr: JSExpression): GenTraversableOnce[JSType] = {
      val funcCtx = FuncCtx(this)
      val exprCtx = ExprCtx(funcCtx, expr, 0)
      findExprType(expr, exprCtx)
    }

    private def hasTypeInfo(t: JSType): Boolean = {
        project.itr.flatMap(project =>
            PropNamePvdr.getNamedProps(t, project)).nonEmpty
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

    def findExprType(expr: JSExpression, exprCtx: ExprCtx): GenTraversableOnce[JSType] = {
        val indent = "  " * exprCtx.depth + "| "
        if (debug) {
            println(indent + "resolving: " + singleLine(expr.getText, 100) + " " + expr.getClass)
        }

        expressionsResolved += 1
        val fromCache = takeFromCache(exprCtx, expr)
        if (fromCache.nonEmpty) {
          fromCache.get.itr()
        } else if (exprCtx.depth > maxDepth) {
            None
        } else if (expressionsResolved >= 7500) {
            None
        } else {
            putToCache(exprCtx, expr, Iterator.empty.mem())
            var gotTypeInfo = false
            val resolved = MainRes.resolveIn(expr, exprCtx).itr
              .filter(t => {
                  gotTypeInfo = hasTypeInfo(t)
                  true
              })
            // no point getting built-in type here, IDEA will show it itself
            val isAtCaret = exprCtx.parent.isEmpty
            val builtIn = getWsType(expr).filter(t => !isAtCaret && !gotTypeInfo)
            var result = frs(resolved, builtIn)
            val mit = result.mem()
            if (debug) {
                println(indent + "resolution: " + mit.itr().map(a => a + " " + a.getClass).toList + " ||| " + singleLine(expr.getText, 350))
            }

            putToCache(exprCtx, expr, mit)
            val cachedTit = mit.itr()
            cachedTit.map(t => typeToDecl.put(t, expr))
            cachedTit
        }
    }
}
