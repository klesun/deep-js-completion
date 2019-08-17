package org.klesun.deep_js_completion.resolvers

import java.util
import java.util.Objects

import com.intellij.lang.javascript.documentation.JSDocumentationUtils
import com.intellij.lang.javascript.psi._
import com.intellij.lang.javascript.psi.ecma6._
import com.intellij.lang.javascript.psi.ecmal4.JSAttributeList.ModifierType
import com.intellij.lang.javascript.psi.ecmal4.{JSAttributeList, JSClass}
import com.intellij.lang.javascript.psi.impl.JSDestructuringParameterImpl
import com.intellij.lang.javascript.psi.jsdoc.JSDocComment
import com.intellij.lang.javascript.psi.jsdoc.impl.JSDocCommentImpl
import com.intellij.lang.javascript.psi.resolve.JSScopeNamesCache
import com.intellij.lang.javascript.psi.types._
import com.intellij.psi.search.GlobalSearchScope
import com.intellij.psi.search.searches.ReferencesSearch
import com.intellij.psi.stubs.StubElement
import com.intellij.psi.{PsiElement, PsiFile, PsiReference}
import org.klesun.deep_js_completion.contexts.IExprCtx
import org.klesun.deep_js_completion.entry.PathStrGoToDecl
import org.klesun.deep_js_completion.helpers.Mt
import org.klesun.deep_js_completion.resolvers.MainRes.getReturns
import org.klesun.deep_js_completion.resolvers.VarRes._
import org.klesun.deep_js_completion.resolvers.var_res.{ArgRes, AssRes, GenericRes}
import org.klesun.deep_js_completion.structures.{JSDeepClassType, JSDeepFunctionTypeImpl}
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

  def findVarUsages(decl: PsiElement): GenTraversableOnce[JSReferenceExpression] = {
    if (Option(decl.getContainingFile).forall(f => f.getName.endsWith(".d.ts")) ||
        Option(decl.getContainingFile).exists(f => f.getTextLength > 3000 * 64)
    ) {
      List()
    } else {
      val scope = GlobalSearchScope.fileScope(decl.getContainingFile)
      val refs: GenTraversableOnce[PsiReference] = try {
        ReferencesSearch.search(decl, scope, false).asScala
      } catch {
        // from my past experience with ReferencesSearch, it is
        // likely to throw random exceptions and cause random hangs...
        case _ => None
      }
      refs.itr.map(ref => ref.getElement)
        .filter(usage => !Objects.equals(usage, decl))
        .flatMap(cast[JSReferenceExpression](_))
        .filter(usage => Objects.equals(decl, usage.resolve()))
    }
  }

  private def findRefUsages(ref: JSReferenceExpression): GenTraversableOnce[JSReferenceExpression] = {
    nit(ref.resolve()).flatMap(decl => findVarUsages(decl))
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
  def resolveForInEl(elDecl: PsiElement): Option[GenTraversableOnce[JSType]] = {
    Option(elDecl.getParent)
      .flatMap(cast[JSForInStatement](_))
      .filter(st => !Objects.equals(elDecl, st.getCollectionExpression))
      .filter(st => st.isForEach)
      .flatMap(st => Option(st.getCollectionExpression))
      .map(arrexpr => ctx.findExprType(arrexpr).itr()
        .flatMap(arrt => ctx.mt().getKey(arrt, None)))
  }

  private def resolveVarSt(varst: JSVarStatement): GenTraversableOnce[JSType] = {
    varst.getChildren.flatMap(cast[JSDocComment](_))
      .flatMap(doc => doc.getTags)
      .map(tag => ArgRes(ctx.subCtxEmpty()).getDocTagComment(tag)).itr()
      .flatMap(txt => ArgRes(ctx.subCtxEmpty()).parseDocExpr(varst, txt)) ++
    resolveForInEl(varst).itr().flatMap(a => a)
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
            ctx.mt().getKey(qualT, keyTOpt)
          })
        types
      case arr: JSDestructuringArray =>
        val types = Option(arr.getParent).itr
          .flatMap(el => resolveDestructEl(el))
          .flatMap(qualT => {
            val keyTOpt = Option(arr.getElements.indexOf(dest))
              .filter(idx => idx > -1)
              .map(idx => new JSStringLiteralTypeImpl(idx + "", true, JSTypeSource.EMPTY))
            ctx.mt().getKey(qualT, keyTOpt)
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
    val isAsync = func.getChildren.flatMap(cast[JSAttributeList](_))
      .exists(lst => lst.hasModifier(ModifierType.ASYNC))
    val docFuncTit = nit(JSDocumentationUtils.findDocComment(func))
      .flatMap(cast[JSDocCommentImpl](_))
      .flatMap(tag => tag.getTags)
      .filter(tag => "return".equals(tag.getName))
      .flatMap(tag => Option(tag.getValue))
      .map(tagVal => tagVal.getText)
      .flatMap(typeText => {
        // the parser does not seem to like {Promise<number>}, it only accepts Promise<number>
        val plain = new JSTypeParser(typeText, JSTypeSource.EMPTY).parseParameterType(true)
        val noBrac = new JSTypeParser(substr(typeText, 1, -1), JSTypeSource.EMPTY).parseParameterType(true)
        cnc(Option(plain), Option(noBrac)).flatMap(dec => Option(dec.getType))
      })
      .map(rett => new JSFunctionTypeImpl(JSTypeSource.EMPTY,
        new util.ArrayList[JSParameterTypeDecorator](), rett))
    val inferFuncTit = getReturns(func).map(r => {
      JSDeepFunctionTypeImpl(func, callCtx => {
        val rett = callCtx.findExprType(r)
        if (!isAsync) rett else rett.itr
          .flatMap(t => Mt.unwrapPromise(t))
          .map(t => Mt.wrapPromise(t))
      }, Some(ctx.func()))
    })
    val wsTit = Option(func.getReturnType).map(rett => {
      new JSFunctionTypeImpl(JSTypeSource.EMPTY,
        new util.ArrayList[JSParameterTypeDecorator](), rett)
    })
    if (shouldTypedefBeIgnored(func)) {
      None
    } else {
      cnc(docFuncTit, inferFuncTit, wsTit)
    }
  }

  // may be defined in a different file unlike resolveAssignment()
  private def resolveFromMainDecl(psi: PsiElement, qualMem: MemIt[JSType]): GenTraversableOnce[JSType] = {
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
          GenericRes(ctx).resolveFunc(tsFunc, qualMem)
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

  private def getDeclarationsFromWs(ref: JSReferenceExpression): GenTraversableOnce[PsiElement] = {
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

  private def assertThisCtorRef(ref: JSReferenceExpression): GenTraversableOnce[JSType] = {
    if (ref.getText.equals("this.constructor")) {
      nit(ref.getQualifier)
        .flatMap(cast[JSThisExpression](_))
        .flatMap(thisPsi => MainRes.getThisCls(thisPsi))
        .flatMap(rec => {
          if (rec.isStatic) {
            None // not sure this.constructor would even be defined in a static call...
          } else {
            Some(JSDeepClassType(rec.clsPsi, ctx.subCtxEmpty()))
          }
        })
    } else {
      None
    }
  }

  def resolve(ref: JSReferenceExpression): GenTraversableOnce[JSType] = {
    val qualMem = nit(ref.getQualifier)
      .flatMap(qual => ctx.findExprType(qual)).mem()
    var tit = cnc(
      qualMem.itr()
        .flatMap(qualT => {
          val keyTOpt = Option(ref.getReferenceName)
            .map(name => new JSStringLiteralTypeImpl(name, true, JSTypeSource.EMPTY))
          val result = ctx.mt().getKey(qualT, keyTOpt)
          result
        })
      ,
      Option(ref.getReferenceName).itr()
        .flatMap(varName => Option(ref.getQualifier).itr()
          .flatMap(assertAtModuleEpxr)
          .flatMap(file => findVarAt(file, varName))
          .flatMap(vari => resolveMainDeclVar(vari)))
      ,
      getDeclarationsFromWs(ref).itr()
        .flatMap(psi => resolveFromMainDecl(psi, qualMem))
      ,
      findRefUsages(ref).itr()
        .flatMap(usage => new AssRes(ctx)
          .resolveAssignmentTo(usage)
            .itr().flatMap(a => a))
      ,
      assertThisCtorRef(ref)
    )
    tit
  }
}
