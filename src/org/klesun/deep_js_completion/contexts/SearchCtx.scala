package org.klesun.deep_js_completion.contexts

import com.intellij.lang.javascript.psi.{JSCallExpression, JSExpression, JSReferenceExpression, JSType}
import com.intellij.lang.javascript.psi.resolve.{JSResolveUtil, JSTypeEvaluator}
import com.intellij.lang.javascript.psi.{JSExpression, JSType}
import org.klesun.deep_js_completion.helpers.Mt
import org.klesun.deep_js_completion.resolvers.MainRes
import org.klesun.lang.Lang._

import scala.collection.{GenTraversable, GenTraversableOnce}
import scala.collection.JavaConverters._

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
            val result = if (resolved.nonEmpty) {
                resolved
            } else {
                val builtIn = getWsType(expr)
                if (debug) {
                    println(indent + "built-in of " + builtIn)
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
