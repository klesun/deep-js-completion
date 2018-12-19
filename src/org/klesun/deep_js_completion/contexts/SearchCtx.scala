package org.klesun.deep_js_completion.contexts

import com.intellij.lang.javascript.psi.{JSExpression, JSType}
import org.klesun.deep_js_completion.resolvers.MainRes

class SearchCtx extends ICtx
{
    var depth = 20

    // for performance measurement
    val expressionsResolved = 0

    val typeToDecl = scala.collection.mutable.Map[JSType, JSExpression]()

    def setDepth(value: Int): SearchCtx = {
        depth = value
        this
    }

    def findExprType(expr: JSExpression): Option[JSType] = {
        if (depth <= 0) {
            None
        } else {
            depth -= 1
            val result = MainRes.resolveIn(expr, this)
            depth += 1
            if (result.isDefined) {
                typeToDecl.put(result.get, expr)
            }
            result
        }
    }
}
