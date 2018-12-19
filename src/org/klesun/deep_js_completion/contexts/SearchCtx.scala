package org.klesun.deep_js_completion.contexts

import com.intellij.lang.javascript.psi.resolve.JSTypeEvaluator
import com.intellij.lang.javascript.psi.types.JSAnyType
import com.intellij.lang.javascript.psi.{JSExpression, JSType}
import org.klesun.deep_js_completion.helpers.Mt
import org.klesun.deep_js_completion.resolvers.MainRes
import org.klesun.lang.Lang.singleLine

class SearchCtx extends ICtx
{
    var maxDepth = 20
    var depth = 0
    var debug = false

    // for performance measurement
    var expressionsResolved = 0

    // for very basic GoTo
    val typeToDecl = scala.collection.mutable.Map[JSType, JSExpression]()
    // caching - to not re-resolve same expression 100500 times
    val exprToResult = scala.collection.mutable.Map[JSExpression, Option[JSType]]()

    def setMaxDepth(value: Int): SearchCtx = {
        maxDepth = value
        this
    }

    private def getWsType(expr: JSExpression) = {
        // TODO: seems that it should be called differently . getExpressionType() looses array element/object key types
        Option(JSTypeEvaluator.getExpressionType(expr))
          .flatMap(res => Option(res.getType))
    }

    def findExprType(expr: JSExpression): Option[JSType] = {
        val indent = "  " * depth + "| "
        if (debug) {
            println(indent + "resolving: " + singleLine(expr.getText, 100) + " " + expr.getClass)
        }

        expressionsResolved += 1
        if (depth > maxDepth) {
            None
        } else if (expressionsResolved >= 7500) {
            None
        } else if (exprToResult.contains(expr)) {
            exprToResult(expr)
        } else {
            exprToResult.put(expr, None)

            depth += 1
            val resolved = MainRes.resolveIn(expr, this)
            depth -= 1

            val result = Mt.mergeTypes(resolved ++ getWsType(expr))

            if (debug) {
                /** @debug */
                println(indent + "resolution: " + resolved)

                if (resolved.isEmpty) {
                    /** @debug */
                    println(indent + "built-in of " + result)
                }
            }

            if (result.isDefined) {
                exprToResult.put(expr, result)
                typeToDecl.put(result.get, expr)
            }
            result
        }
    }
}
