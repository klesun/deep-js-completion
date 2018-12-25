package org.klesun.deep_js_completion.contexts

import com.intellij.lang.javascript.psi.resolve.JSTypeEvaluator
import com.intellij.lang.javascript.psi.{JSExpression, JSType}
import org.klesun.deep_js_completion.helpers.Mt
import org.klesun.deep_js_completion.resolvers.MainRes
import org.klesun.lang.Lang.singleLine

class SearchCtx(
    val maxDepth: Integer = 20,
    val debug: Boolean = false,
) {

    // for performance measurement
    var expressionsResolved = 0

    // for very basic GoTo
    val typeToDecl = scala.collection.mutable.Map[JSType, JSExpression]()
    // caching - to not re-resolve same expression 100500 times
    val exprToResult = scala.collection.mutable.Map[JSExpression, Option[JSType]]()

    private def getWsType(expr: JSExpression) = {
        // TODO: seems that it should be called differently . getExpressionType() looses array element/object key types
        Option(JSTypeEvaluator.getExpressionType(expr))
          .flatMap(res => Option(res.getType))
    }

    def findExprType(expr: JSExpression): Option[JSType] = {
      val funcCtx = FuncCtx(this)
      val exprCtx = ExprCtx(funcCtx, expr, 0)
      findExprType(expr, exprCtx)
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
        } else if (exprToResult.contains(expr)) {
            exprToResult(expr)
        } else {
            exprToResult.put(expr, None)
            val resolved = MainRes.resolveIn(expr, exprCtx)
            val result = Mt.mergeTypes(resolved ++ getWsType(expr))
            if (result.isDefined) {
                exprToResult.put(expr, result)
                typeToDecl.put(result.get, expr)
            }
            if (debug) {
                /** @debug */
                println(indent + "resolution: " + resolved + " ||| " + singleLine(expr.getText, 350))
                if (resolved.isEmpty) {
                    /** @debug */
                    println(indent + "built-in of " + result)
                }
            }
            result
        }
    }
}
