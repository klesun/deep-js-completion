package org.klesun.deep_js_completion.contexts

import com.intellij.lang.javascript.psi.resolve.JSTypeEvaluator
import com.intellij.lang.javascript.psi.{JSCallExpression, JSExpression, JSReferenceExpression, JSType}
import com.intellij.openapi.project.Project
import org.klesun.deep_js_completion.completion_providers.PropNamePvdr
import org.klesun.deep_js_completion.resolvers.MainRes
import org.klesun.lang.DeepJsLang._

import scala.collection.GenTraversableOnce

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
    val exprToResult = scala.collection.mutable.Map[JSExpression, MemIt[JSType]]()

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
            PropNamePvdr.getProps(t, project)).nonEmpty
    }

    def findExprType(expr: JSExpression, exprCtx: ExprCtx): GenTraversableOnce[JSType] = {
        val indent = "  " * exprCtx.depth + "| "
        if (debug) {
            println(indent + "resolving: " + singleLine(expr.getText, 100) + " " + expr.getClass)
        }

        expressionsResolved += 1
        if (exprToResult.contains(expr)) {
            exprToResult(expr).itr()
        } else if (exprCtx.depth > maxDepth) {
            None
        } else if (expressionsResolved >= 7500) {
            None
        } else {
            exprToResult.put(expr, Iterator.empty.mem())
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

            exprToResult.put(expr, mit)
            val cachedTit = mit.itr()
            cachedTit.map(t => typeToDecl.put(t, expr))
            cachedTit
        }
    }
}
