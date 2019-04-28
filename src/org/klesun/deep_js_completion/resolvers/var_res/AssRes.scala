package org.klesun.deep_js_completion.resolvers.var_res

import com.intellij.lang.javascript.psi.JSRecordType.TypeMember
import com.intellij.lang.javascript.psi.types.{JSArrayTypeImpl, JSRecordTypeImpl, JSTypeSource}
import com.intellij.lang.javascript.psi._
import org.klesun.deep_js_completion.contexts.IExprCtx
import org.klesun.deep_js_completion.helpers.{Mkt, Mt}
import org.klesun.deep_js_completion.resolvers
import org.klesun.deep_js_completion.structures.{DeepIndexSignatureImpl, JSDeepMultiType}
import org.klesun.lang.DeepJsLang._

import scala.collection.JavaConverters._
import scala.collection.{GenTraversableOnce, mutable}

/**
 * resolves assignment to a var reference, including key assignment, .push(), etc...
 */
class AssRes(val ctx: IExprCtx) {
  private val occurrences = new mutable.HashSet[JSExpression]()

  private def resolveAssignmentNoCirc(usage: JSExpression): GenTraversableOnce[JSType] = {
    nit(usage.getParent).flatMap {
      case superRef: JSReferenceExpression => frs(Iterator.empty
        // someVar.push(value)
        , nit(superRef.getReferenceName)
          .filter(refName => List("push", "unshift").contains(refName))
          .flatMap(refName => Option(superRef.getParent))
          .flatMap(cast[JSCallExpression](_))
          .flatMap(call => call.getArguments.lift(0))
          .flatMap(value => ctx.findExprType(value))
          .map(elT => new JSArrayTypeImpl(elT, JSTypeSource.EMPTY))
        // someVar.someKey = 123
        , nit(superRef.getReferenceName)
          .flatMap(name => resolveAssignmentTo(superRef).itr()
            .map(valt => Mt.mkProp(name, Some(valt), Some(superRef)))
            .map((prop: TypeMember) => new JSRecordTypeImpl(JSTypeSource.EMPTY, List(prop).asJava)))
      )
      // someVar[i] = value
      case indexing: JSIndexedPropertyAccessExpression =>
        Option(indexing.getIndexExpression)
          .filter(lit => usage equals indexing.getQualifier).itr()
          .flatMap(lit => ctx.limitResolveDepth(10, lit))
          .map(keyt => {
            val valts = resolveAssignmentTo(indexing)
            val valT = JSDeepMultiType(valts.mem())
            DeepIndexSignatureImpl(keyt, valT, Some(indexing))
          })
          .map((prop: TypeMember) => new JSRecordTypeImpl(JSTypeSource.EMPTY, List(prop).asJava))
      // var someVar = null;
      // someVar = initializeSomething()
      case usage: JSDefinitionExpression =>
        nit(usage.getParent)
          .flatMap(cast[JSAssignmentExpression](_))
          .flatMap(defi => Option(defi.getROperand))
          .flatMap(expr => ctx.findExprType(expr))
      case arrLit: JSArrayLiteralExpression =>
        // for ([k,v] of arr) {}
        // [el1, el2] = tuple;
        val elOrder = arrLit.getExpressions.indexOf(usage)
        val kit: GenTraversableOnce[JSType] =
          if (elOrder > -1) Mkt.str(elOrder + "") else None

        val artit = resolvers.VarRes(ctx).resolveForInEl(arrLit)
        artit.itr().flatMap(elt => ctx.mt().getKey(elt, kit))
      case untyped =>
        //Console.println("zhopa unsupported assignment destination " + untyped.getClass + " " + untyped.getText)
        List[JSType]()
    }
  }

  def resolveAssignmentTo(usage: JSExpression): GenTraversableOnce[JSType] = {
    // not sure, but I guess there can't be circular references here
    resolveAssignmentNoCirc(usage)
  }
}
