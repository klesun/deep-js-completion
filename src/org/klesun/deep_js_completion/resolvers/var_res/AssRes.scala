package org.klesun.deep_js_completion.resolvers.var_res

import com.intellij.lang.javascript.psi._
import com.intellij.lang.javascript.psi.types.{JSArrayTypeImpl, JSRecordTypeImpl, JSTypeSource}
import org.klesun.deep_js_completion.contexts.IExprCtx
import org.klesun.deep_js_completion.helpers.{Mkt, Mt}
import org.klesun.deep_js_completion.resolvers.VarRes
import org.klesun.deep_js_completion.structures.{DeepIndexSignatureImpl, JSDeepMultiType}
import org.klesun.lang.DeepJsLang._

import scala.collection.GenTraversableOnce
import scala.collection.JavaConverters._

/**
 * resolves assignment to a var reference, including key assignment, .push(), etc...
 */
class AssRes(val ctx: IExprCtx) {
  def resolveAssignmentTo(usage: JSExpression): Option[GenTraversableOnce[JSType]] = {
    Option(usage.getParent).flatMap {
      case superRef: JSReferenceExpression => frsOpt(None
        // someVar.push(value)
        , Option(superRef.getReferenceName)
          .filter(refName => List("push", "unshift").contains(refName))
          .flatMap(refName => Option(superRef.getParent))
          .flatMap(cast[JSCallExpression](_))
          .flatMap(call => call.getArguments.lift(0))
          .map(value => ctx.findExprType(value).itr()
            .map(elT => new JSArrayTypeImpl(elT, JSTypeSource.EMPTY)))
        // someVar.someKey = 123
        , Option(superRef.getReferenceName)
          .flatMap(name => resolveAssignmentTo(superRef)
            .map(valts => {
              val prop = Mt.mkProp(name, valts, Some(superRef))
              val rect = new JSRecordTypeImpl(JSTypeSource.EMPTY, List(prop).asJava)
              Some(rect)
            }))
      )
      // someVar[i] = value
      case indexing: JSIndexedPropertyAccessExpression =>
        Option(indexing.getIndexExpression)
          .filter(lit => usage equals indexing.getQualifier)
          .flatMap(lit => resolveAssignmentTo(indexing)
            .map(valts => {
              val keyts = ctx.limitResolveDepth(10, lit)
              val keyt = JSDeepMultiType(keyts.mem())
              val valt = JSDeepMultiType(valts.mem())
              val prop = DeepIndexSignatureImpl(keyt, valt, Some(indexing))
              val rect = new JSRecordTypeImpl(JSTypeSource.EMPTY, List(prop).asJava)
              Some(rect)
            }))
      // var someVar = null;
      // someVar = initializeSomething()
      case usage: JSDefinitionExpression =>
        Option(usage.getParent)
          .flatMap(cast[JSAssignmentExpression](_))
          .flatMap(defi => Option(defi.getROperand))
          .map(expr => ctx.findExprType(expr))
      case arrLit: JSArrayLiteralExpression =>
        // for ([k,v] of arr) {}
        // [el1, el2] = tuple;
        val elOrder = arrLit.getExpressions.indexOf(usage)
        val kit: GenTraversableOnce[JSType] =
          if (elOrder > -1) Mkt.str(elOrder + "") else None

        VarRes(ctx).resolveForInEl(arrLit).map(artit => {
          artit.itr().flatMap(elt => ctx.mt().getKey(elt, kit))
        })
      case untyped =>
        //Console.println("Unsupported assignment destination " + untyped.getClass + " " + untyped.getText)
        None
    }
  }
}
