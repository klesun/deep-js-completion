package org.klesun.deep_js_completion.contexts

import com.intellij.lang.javascript.psi.resolve.JSTypeEvaluator
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

    val typeToDecl = scala.collection.mutable.Map[JSType, JSExpression]()

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
        } else {
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
                typeToDecl.put(result.get, expr)
            }
            result
        }
    }
}
