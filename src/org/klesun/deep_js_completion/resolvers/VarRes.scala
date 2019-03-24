package org.klesun.deep_js_completion.resolvers

import java.util
import java.util.Objects

import com.intellij.lang.javascript.psi.JSRecordType.TypeMember
import com.intellij.lang.javascript.psi._
import com.intellij.lang.javascript.psi.ecma6._
import com.intellij.lang.javascript.psi.ecmal4.JSClass
import com.intellij.lang.javascript.psi.impl.JSDestructuringParameterImpl
import com.intellij.lang.javascript.psi.jsdoc.JSDocComment
import com.intellij.lang.javascript.psi.resolve.JSScopeNamesCache
import com.intellij.lang.javascript.psi.types._
import com.intellij.psi.stubs.StubElement
import com.intellij.psi.{PsiElement, PsiFile}
import org.klesun.deep_js_completion.contexts.IExprCtx
import org.klesun.deep_js_completion.entry.PathStrGoToDecl
import org.klesun.deep_js_completion.helpers.{Mkt, Mt}
import org.klesun.deep_js_completion.resolvers.VarRes._
import org.klesun.deep_js_completion.resolvers.var_res.{ArgRes, GenericRes}
import org.klesun.deep_js_completion.structures.{DeepIndexSignatureImpl, JSDeepClassType, JSDeepMultiType}
import org.klesun.lang.DeepJsLang
import org.klesun.lang.DeepJsLang._

import scala.collection.GenTraversableOnce
import scala.collection.JavaConverters._

object VarRes {

  def findVarAt(file: PsiElement, name: String): GenTraversableOnce[JSVariable] = {
    val els = JSScopeNamesCache.findNamedElementsInStubScope(name, file).asScala
    cast[JSElement](file).itr.flatMap(file => {
      val hap = JSScopeNamesCache.getOrCreateNamesForScope(file)
      els.flatMap(cast[JSVariable](_)) ++ hap.getValues
        .flatMap(cast[JSVariable](_))
        .flatMap(ref => Option(ref.getInitializer))
        .flatMap(v => findVarAt(v, name))
    })
  }

  // possibly there is no point in a different implementation for
  // findVarAt() since we traverse whole tree anyway there as well...
  // TODO: include vars defined in methods and anonymous functions
  def findAllVarsAt(file: PsiElement): GenTraversableOnce[JSVariable] = {
    cast[JSElement](file).itr().flatMap(file => {
      val hap = JSScopeNamesCache.getOrCreateNamesForScope(file)
      val scopeVars = hap.getValues
        .flatMap(cast[JSVariable](_)).mem()
      scopeVars.itr() ++ scopeVars.itr()
        .flatMap(ref => Option(ref.getInitializer))
        .flatMap(v => findAllVarsAt(v))
    })
  }

  def findVarUsages(decl: PsiElement, name: String): GenTraversableOnce[JSReferenceExpression] = {
    // maybe this could be used instead: JSScopeNamesUsages
    if (Option(decl.getContainingFile).forall(f => f.getName.endsWith(".d.ts")) ||
        Option(decl.getContainingFile).exists(f => f.getTextLength > 3000 * 64)
    ) {
      List()
    } else {
      val scope: PsiElement = DeepJsLang.findParent[JSFunctionExpression](decl)
        .getOrElse(decl.getContainingFile)
      if (scope.getTextLength > 3000 * 64) {
        // eliminate .min.js files and such - anything longer than ~ 3000 lines
        List()
      } else {
        val t1 = System.nanoTime
        val result = DeepJsLang.findChildren[JSReferenceExpression](scope)
          .filter(usage => Objects.equals(usage.getReferenceName, name))
          .filter(usage => !Objects.equals(usage, decl))
          .filter(usage => Objects.equals(decl, usage.resolve()))
        val duration = (System.nanoTime - t1) / 1e9d
        result
      }
    }
  }

  private def findRefUsages(ref: JSReferenceExpression): GenTraversableOnce[JSReferenceExpression] = {
    nit(ref.resolve()).flatMap(decl => findVarUsages(decl, ref.getReferenceName))
      .filter(usage => !usage.equals(ref))
  }
}

/**
 * resolves variable type
 */
case class VarRes(ctx: IExprCtx) {

  /**
   * @param elDecl - either VarStatement, Variable (declared above) or destructuring array
   * @return - type of element of the iterated array
   */
  private def resolveForInEl(elDecl: PsiElement): GenTraversableOnce[JSType] = {
    nit(elDecl.getParent)
      .flatMap(cast[JSForInStatement](_))
      .filter(st => !Objects.equals(elDecl, st.getCollectionExpression))
      .filter(st => st.isForEach)
      .flatMap(st => Option(st.getCollectionExpression))
      .flatMap(arrexpr => ctx.findExprType(arrexpr))
      .flatMap(arrt => Mt.getKey(arrt, None))
  }

  private def resolveAssignmentTo(usage: JSExpression): GenTraversableOnce[JSType] = {
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
        Option(indexing.getQualifier)
          .filter(qual => usage equals qual).itr
          .flatMap(qual => resolveAssignmentTo(indexing))
          .map(valT => {
            val keyts = ctx.findExprType(indexing.getIndexExpression)
            val keyt = JSDeepMultiType(keyts.mem())
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

        val artit = resolveForInEl(arrLit)
        artit.itr().flatMap(elt => Mt.getKey(elt, kit))
      case untyped =>
        //Console.println("zhopa unsupported assignment destination " + untyped.getClass + " " + untyped.getText)
        List[JSType]()
    }
  }

  private def resolveVarSt(varst: JSVarStatement): GenTraversableOnce[JSType] = {
    varst.getChildren.flatMap(cast[JSDocComment](_))
      .flatMap(doc => doc.getTags)
      .map(tag => ArgRes(ctx.subCtxEmpty()).getDocTagComment(tag))
      .flatMap(txt => ArgRes(ctx.subCtxEmpty()).parseDocExpr(varst, txt)) ++
    resolveForInEl(varst)
  }

  private def resolveDestructEl(el: PsiElement): GenTraversableOnce[JSType] = {
    el match {
      // let doStuff = ({a, b}) => {...};
      case para: JSDestructuringParameterImpl => ArgRes(ctx).resolve(para)
      // let {a, b} = getObj();
      case obj: JSDestructuringElement =>
        nit(obj.getInitializer)
          .flatMap(qual => ctx.findExprType(qual)) ++
        nit(obj.getParent).itr
          .flatMap(cast[JSVarStatement](_))
          .flatMap(st => resolveVarSt(st))
      case _ => None
    }
  }

  def resolveMainDeclVar(dest: JSVariable): GenTraversableOnce[JSType] = {
    nit(dest.getInitializer)
      .flatMap(expr => ctx.findExprType(expr)) ++
    nit(dest.getParent).itr.flatMap {
      case prop: JSDestructuringShorthandedProperty =>
        val types = Option(prop.getParent)
          .flatMap(cast[JSDestructuringObject](_))
          .flatMap(obj => Option(obj.getParent)).itr
          .flatMap(resolveDestructEl)
          .flatMap(qualT => {
            val keyTOpt = Option(dest.getName)
              .map(name => new JSStringLiteralTypeImpl(name, true, JSTypeSource.EMPTY))
            Mt.getKey(qualT, keyTOpt)
          })
        types
      case arr: JSDestructuringArray =>
        val types = Option(arr.getParent).itr
          .flatMap(el => resolveDestructEl(el))
          .flatMap(qualT => {
            val keyTOpt = Option(arr.getElements.indexOf(dest))
              .filter(idx => idx > -1)
              .map(idx => new JSStringLiteralTypeImpl(idx + "", true, JSTypeSource.EMPTY))
            Mt.getKey(qualT, keyTOpt)
          })
        types
      case varst: JSVarStatement => resolveVarSt(varst)
      case _ => None
    }
  }

  private def shouldTypedefBeIgnored(tsFunc: JSFunction): Boolean = {
    // es2015 d.ts has some weird return type - Promise<TResult1 | TResult2>,
    // it results in irrelevant options, so I'm overriding it here
    ("then" equals tsFunc.getName) &&
      List("lib.es2015.promise.d.ts", "lib.es5.d.ts").contains(tsFunc.getContainingFile.getName)
  }

  def resolveFunc(func: JSFunction): GenTraversableOnce[JSType] = {
    MainRes.getReturns(func)
      .filter(ret => !shouldTypedefBeIgnored(func))
      .flatMap(expr => ctx.findExprType(expr))
      .map(rett => new JSFunctionTypeImpl(JSTypeSource.EMPTY,
        new util.ArrayList[JSParameterTypeDecorator](), rett))
  }

  // may be defined in a different file unlike resolveAssignment()
  private def resolveFromMainDecl(psi: PsiElement): GenTraversableOnce[JSType] = {
    psi match {
      case para: JSParameter => ArgRes(ctx).resolve(para) ++ resolveMainDeclVar(para)
      case dest: JSVariable => resolveMainDeclVar(dest)
      case prop: JSProperty => nit(prop.getValue)
        .flatMap(expr => ctx.findExprType(expr))
      case prop: JSDefinitionExpression => nit(prop.getExpression)
        .flatMap(expr => ctx.findExprType(expr))
      case tsFunc: TypeScriptFunctionSignature => {
        if (shouldTypedefBeIgnored(tsFunc)) {
          None
        } else {
          GenericRes(ctx).resolveFunc(tsFunc)
        }
      }
      case func: JSFunction => resolveFunc(func)
      case cls: JSClass[StubElement[_]] =>
        cast[JSClass[StubElement[_]]](cls)
          .map(cls => {
            val clst = JSDeepClassType(cls, ctx.subCtxEmpty())
            clst
          })
      case _ =>
        //println("Unsupported var declaration - " + psi.getClass + " " + psi.getText)
        None
    }
  }

  private def getDeclarations(ref: JSReferenceExpression): GenTraversableOnce[PsiElement] = {
    val isProp = ref.getQualifier != null
    // it would be nice to always use es2018 instead of es2015 somehow
    val psis = Option(ref.resolve()).itr
      .filter(decl => {
        val isDts = Option(decl.getContainingFile).exists(f => f.getName endsWith ".d.ts")
        // skip definitions that are actually just random props in project with same name
        !isProp || isDts
      })
    psis
  }

  // an imaginary at('Module.js') function used in docs to
  // specify any var defined in the file with the following name
  private def assertAtModuleEpxr(qual: JSExpression): GenTraversableOnce[PsiFile] = {
    cast[JSCallExpression](qual)
      .filter(call => Option(call.getMethodExpression).exists(meth => meth.getText equals "at"))
      .flatMap(call => call.getArguments.lift(0)).itr
      .flatMap(PathStrGoToDecl.getReferencedFileAnyDir)
  }

  def resolve(ref: JSReferenceExpression): GenTraversableOnce[JSType] = {
    (
      nit(ref.getQualifier)
        .flatMap(qual => ctx.findExprType(qual))
        .flatMap(qualT => {
          val keyTOpt = Option(ref.getReferenceName)
            .map(name => new JSStringLiteralTypeImpl(name, true, JSTypeSource.EMPTY))
          val result = Mt.getKey(qualT, keyTOpt)
          result
        })
      ++
      Option(ref.getReferenceName).itr()
        .flatMap(varName => Option(ref.getQualifier).itr()
          .flatMap(assertAtModuleEpxr)
          .flatMap(file => findVarAt(file, varName))
          .flatMap(vari => resolveMainDeclVar(vari)))
      ++
      getDeclarations(ref).itr()
        .flatMap(psi => resolveFromMainDecl(psi))
      ++
      findRefUsages(ref).itr()
        .flatMap(usage => resolveAssignmentTo(usage))
    )
  }
}
