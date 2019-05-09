package org.klesun.deep_js_completion.resolvers.var_res

import java.util
import java.util.Objects

import com.intellij.lang.javascript.JavascriptLanguage
import com.intellij.lang.javascript.documentation.JSDocumentationUtils
import com.intellij.lang.javascript.psi._
import com.intellij.lang.javascript.psi.ecma6.impl.TypeScriptFunctionSignatureImpl
import com.intellij.lang.javascript.psi.impl.{JSExpressionStatementImpl, JSReferenceExpressionImpl}
import com.intellij.lang.javascript.psi.jsdoc.JSDocTag
import com.intellij.lang.javascript.psi.jsdoc.impl.JSDocCommentImpl
import com.intellij.lang.javascript.psi.types._
import com.intellij.psi.impl.source.tree.LeafPsiElement
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiElement, PsiFile, PsiFileFactory, PsiWhiteSpace}
import org.klesun.deep_js_completion.contexts.{IExprCtx, IFuncCtx}
import org.klesun.deep_js_completion.entry.PathStrGoToDecl
import org.klesun.deep_js_completion.helpers.{Mkt, Mt}
import org.klesun.deep_js_completion.resolvers.var_res.ArgRes._
import org.klesun.deep_js_completion.resolvers.{MainRes, VarRes}
import org.klesun.deep_js_completion.structures.{EInstType, JSDeepFunctionTypeImpl, JSDeepModuleTypeImpl, JSDeepMultiType}
import org.klesun.lang.DeepJsLang
import org.klesun.lang.DeepJsLang.{cast, nit}

import scala.collection.GenTraversableOnce
import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer
import org.klesun.lang.DeepJsLang._

object ArgRes {

  private def grabClosureCtxs(leafCtx: IFuncCtx): GenTraversableOnce[IFuncCtx] = {
    var next: Option[IFuncCtx] = Some(leafCtx)
    var result: GenTraversableOnce[IFuncCtx] = List()
    while (next.isDefined) {
      result = next ++ result
      next = next.get.getClosureCtx()
    }
    result
  }

  // based on https://expressjs.com/en/api.html#req, though I witnessed some of the described fields (protocol, cookies, signedCookies, fresh, stale) being absent...
  private def makeExpressRqType(): GenTraversableOnce[JSType] = {
    // probably would be good to use the definition in node_modules/express/lib/request.js one day...
    val extension = Mkt.assoc(List(
      ("body", () => Mkt.assoc(List(
        ("start", () => Mkt.str("3")),
        ("length", () => Mkt.str("25")),
        ("order", () => Mkt.str("dt desc")),
      ))),
      ("app", () => Some(JSDeepModuleTypeImpl("express", EInstType.Called))),
      ("baseUrl", () => Mkt.str("")),
      ("hostname", () => Mkt.str("example.com:3000")),
      ("ip", () => Mkt.str("127.0.0.1")),
      ("ips", () => Mkt.arr(Mkt.str("client", "proxy1", "proxy2"))),
      ("method", () => Mkt.str("GET", "POST", "OPTIONS", "PUT", "DELETE", "CONNECT", "TRACE", "PATCH")),
      ("originalUrl", () => Mkt.str("/search?q=something")),
      ("params", () => Mkt.assoc(List(
        ("dir1", () => Mkt.str("user")),
        ("dir2", () => Mkt.str("add")),
        ("file", () => Mkt.str("role")),
      ))),
      ("path", () => Mkt.str("/user/add/role")),
      ("protocol", () => Mkt.str("http", "https", "")),
      ("query", () => Mkt.assoc(List(
        ("order", () => Mkt.str("desc")),
        ("color", () => Mkt.str("blue")),
        ("type", () => Mkt.str("converse")),
      ))),
      ("route", () => Mkt.assoc(List(
        ("path", () => Mkt.str("/user/:id?")),
        ("stack", () => Mkt.arr(Mkt.assoc(List(
          ("handle", () => Mkt.func()),
          ("name", () => Mkt.str("userIdHandler")),
          ("params", () => Mkt.any()),
          ("path", () => Mkt.any()),
          ("keys", () => Mkt.arr(List())),
          ("regexp", () => Mkt.regexp()),
          ("method", () => Mkt.str("get", "post")),
        )))),
        ("methods", () => Mkt.assoc(List(
          ("get", () => Mkt.bool()),
          ("post", () => Mkt.bool()),
        ))),
      ))),
      // did not see following in the actual output, even though they are documented...
      ("secure", () => Mkt.bool()),
      ("cookies", () => Mkt.assoc(List(
        ("PHPSESSID", () => Mkt.str("qwe213zxcasd")),
        ("CONSENT", () => Mkt.str("YES+US.en+20150628-20-0")),
      ))),
      ("signedCookies", () => Mkt.assoc(List(
        ("user", () => Mkt.str("tobi")),
      ))),
      ("fresh", () => Mkt.bool()),
      ("stale", () => Mkt.bool()),
      ("subdomains", () => Mkt.arr(Mkt.str("ferrets", "tobi"))),
      ("xhr", () => Mkt.bool()),
      ("accepts", () => Mkt.func()),
      ("acceptsCharsets", () => Mkt.func()),
      ("acceptsEncodings", () => Mkt.func()),
      ("acceptsLanguages", () => Mkt.func()),
      ("get", () => Mkt.func()),
      ("is", () => Mkt.func()),
      ("range", () => Mkt.func()),
    ))
    extension ++ Mkt.inst("http.IncomingMessage")
  }

  private def makeExpressRsType(): GenTraversableOnce[JSType] = {
    // probably would be good to use the definition in node_modules/express/lib/response.js one day...
    val extension = Mkt.assoc(List(
      ("send", () => Mkt.func()),
      ("sendStatus", () => Mkt.func()),
      ("setHeader", () => Mkt.func()), // not documented for some reason
      ("append", () => Mkt.func()),
      ("attachment", () => Mkt.func()),
      ("app", () => Some(JSDeepModuleTypeImpl("express", EInstType.Called))),
      ("headersSent", () => Mkt.bool()),
      ("locals", () => Mkt.assoc(List())),
      ("cookie", () => Mkt.func()),
      ("clearCookie", () => Mkt.func()),
      ("download", () => Mkt.func()),
      ("end", () => Mkt.func()),
      ("format", () => Mkt.func()),
      ("get", () => Mkt.func()),
      ("json", () => Mkt.func()),
      ("jsonp", () => Mkt.func()),
      ("links", () => Mkt.func()),
      ("location", () => Mkt.func()),
      ("redirect", () => Mkt.func()),
      ("render", () => Mkt.func()),
      ("sendFile", () => Mkt.func()),
      ("set", () => Mkt.func()),
      ("status", () => Mkt.func()),
      ("type", () => Mkt.func()),
      ("vary", () => Mkt.func()),
    ))
    extension ++ Mkt.inst("http.ServerResponse")
  }

  private def resolveTsFuncArgArg(objt: Option[JSType], tsFuncDecl: TypeScriptFunctionSignatureImpl, ctx: IExprCtx, inlineFuncArgOrder: Int, argOrder: Int): GenTraversableOnce[JSType] = {
    new GenericRes(ctx).resolveFuncArg(objt.mem(), ctx, inlineFuncArgOrder, tsFuncDecl)
      .itr.flatMap(cast[JSFunctionTypeImpl](_))
      .flatMap(funct => funct.getParameters.asScala.lift(argOrder))
      .flatMap(arg => Option(arg.getType))
  }

  private def findReferencedFunc(funcVarDecl: PsiElement): Option[JSFunction] = {
    // TODO: add circular references check... and implementation
    funcVarDecl match {
      // TODO: test and uncomment
      //case tsFuncDecl: TypeScriptFunctionSignatureImpl => {
      //  resolveTsFuncArgArg(objt, tsFuncDecl, outerCallCtx, inlineFuncArgOrder, argOrder)
      //}
      case varDef: JSDefinitionExpression =>
        Option(varDef.getParent)
          .flatMap(cast[JSAssignmentExpression](_))
          .flatMap(defi => Option(defi.getROperand))
          .flatMap(cast[JSFunction](_))
      case _ =>
        None
    }
  }

  private def resolvePrivateFuncUsages(par: JSParameter, ctx: IExprCtx, argOrder: Int): It[JSType] = {
    VarRes.findVarUsages(par).itr()
        .flatMap(usage => Option(usage.getParent)
          .flatMap(cast[JSCallExpression](_))
          .filter(call => usage eq call.getMethodExpression))
        .flatMap(call => call.getArguments.lift(argOrder))
        .flatMap(value => ctx.subCtxEmpty().findExprType(value))
  }
}

case class ArgRes(ctx: IExprCtx) {

  private def getFuncParam(paramOrder: Int, ref: JSReferenceExpressionImpl): It[JSParameter] = {
    val wsFuncDefs = nit(ref.resolve())
      .flatMap(funcVarDecl => findReferencedFunc(funcVarDecl))

    val deepFuncDefs = ctx.subCtxEmpty().findExprType(ref).itr()
      .flatMap(cast[JSDeepFunctionTypeImpl](_))
      .map(ft => ft.funcPsi)

    cnc(wsFuncDefs, deepFuncDefs)
      .flatMap(callerFunc => callerFunc.getParameters.lift(paramOrder)
        .flatMap(callerArg => cast[JSParameter](callerArg)))
  }

  private def getInlineFuncArgType(func: JSFunction, argOrder: Integer): GenTraversableOnce[JSType] = {
    nit(func.getParent)
      .flatMap(cast[JSArgumentList](_))
      .flatMap(argList => nit(argList.getArguments.indexOf(func))
        .flatMap(inlineFuncArgOrder => Option(true)
          .flatMap(ok => Option(argList.getParent))
          .flatMap(cast[JSCallExpression](_)).itr
          .flatMap(call => nit(call.getMethodExpression)
            .flatMap(cast[JSReferenceExpressionImpl](_))
            .flatMap(ref => {
              val objt = nit(ref.getQualifier)
                .flatMap(obj => ctx.findExprType(obj))
              val outerCallCtx = ctx.subCtxDirect(call)

              (getFuncParam(inlineFuncArgOrder, ref).flatMap(par =>
                resolvePrivateFuncUsages(par, ctx.subCtxEmpty(), argOrder))

              // following built-in functions are hardcoded and probably
              // are not needed once generic parsing works properly...
              // should check and remove each of them one by one from here

              ++
              nit(ref.getQualifier)
                .filter(expr => argOrder == 0)
                .filter(expr => List("forEach", "map", "filter", "sort").contains(ref.getReferencedName))
                .flatMap(expr => ctx.findExprType(expr))
                .flatMap(arrt => ctx.mt().getKey(arrt, None)).itr
                ++
              nit(ref.getQualifier)
                .filter(expr => argOrder == 1)
                .filter(expr => List("reduce").contains(ref.getReferencedName))
                .flatMap(expr => ctx.findExprType(expr))
                .flatMap(arrt => ctx.mt().getKey(arrt, None)).itr
                ++
              nit(ref.getQualifier)
                // func arg order does not matter, it may be 0 or 1, maybe something else as well
                .filter(expr => List("use", "get", "post").contains(ref.getReferencedName))
                .flatMap(expr => ctx.findExprType(expr)).itr
                .flatMap(t => Mt.flattenTypes(t))
                .flatMap(cast[JSDeepModuleTypeImpl](_))
                .filter(modt => ("express" equals modt.name) && (modt.instType equals EInstType.Called))
                .flatMap(arrt => {
                  if (argOrder == 0) {
                    // request obj
                    makeExpressRqType()
                  } else if (argOrder == 1) {
                    // response obj
                    makeExpressRsType()
                  } else {
                    None
                  }
                }: GenTraversableOnce[JSType])
                ++
                nit(ref.getQualifier)
                  .filter(expr => argOrder == 0)
                  .filter(expr => List("then").contains(ref.getReferencedName))
                  .flatMap(expr => ctx.findExprType(expr))
                  .flatMap(promiset => Mt.unwrapPromise(promiset))
              )
            })
          )
        )
      )
  }

  // private function completion (based on scanning current
  // file for usages and taking what is passed to the function)
  private def getPrivateFuncArgType(func: JSFunction, argOrder: Integer): GenTraversableOnce[JSType] = {
    if (ctx.func().areArgsKnown()) {
      None
    } else {
      val usages = cnc(
        nit(func.getParent)
          .flatMap(cast[JSVariable](_))
          .flatMap(vari => VarRes.findVarUsages(vari)),
        VarRes.findVarUsages(func),
      )
      usages.flatMap(usage => nit(usage.getParent)
          .flatMap(cast[JSCallExpression](_))
          .filter(call => usage eq call.getMethodExpression))
          .flatMap(call => call.getArguments.lift(argOrder))
          .flatMap(value => ctx.subCtxEmpty().findExprType(value))
    }
  }

  private def getCtxArgType(func: JSFunction, para: JSParameterListElement): GenTraversableOnce[JSType] = {
    val order = func.getParameters.indexOf(para)
    grabClosureCtxs(ctx.func()).itr
      .find(_.getClosurePsi().exists(_ equals func)).itr
      .flatMap(ctx => if (para.isRest) Some(ctx.getSpreadArg) else ctx.getArg(order))
  }

  private def resolveRequireJsSupplierDef(file: PsiFile): GenTraversableOnce[JSType] = {
    PsiTreeUtil.findChildrenOfType(file, classOf[JSCallExpression]).asScala
      .find(assi => assi.getText.startsWith("define("))
      .flatMap(call => call.getArguments.lift(1)).itr
      .flatMap(moduleSupplier => ctx.findExprType(moduleSupplier))
  }

  def resolveCommonJsFormatDef(file: PsiFile): GenTraversableOnce[JSType] = {
    val types = file.getChildren
      .flatMap(cast[JSExpressionStatement](_))
      .flatMap(_.getChildren)
      .flatMap(cast[JSAssignmentExpression](_)).itr()
      .flatMap(ass => nit(ass.getROperand)
        .flatMap(value => nit(ass.getLOperand)
          .flatMap(vari => {
            val txt = Option(vari.getText).getOrElse("")
            if (txt startsWith "module.exports") {
              ctx.findExprType(value)
            } else if (txt startsWith "exports.") {
              cast[JSDefinitionExpression](vari)
                .filter(ref => Option(vari.getText).exists(txt => txt startsWith "exports."))
                .flatMap(defi => Option(defi.getFirstChild))
                .flatMap(cast[JSReferenceExpressionImpl](_))
                .flatMap(ref => Option(ref.getReferencedName))
                .map(name => Mt.mkProp(name, ctx.findExprType(value), Some(vari)))
                .map(prop => new JSRecordTypeImpl(JSTypeSource.EMPTY, List(prop).asJava))
            } else {
              None
            }
          })
        )
      )
    types
  }

  private def resolveRequireJsFormatDef(file: PsiFile): GenTraversableOnce[JSType] = {
    val types = resolveRequireJsSupplierDef(file)
    types.itr().flatMap(sup => Mt.getReturnType(sup, ctx.subCtxEmpty()))
  }

  def getDocTagComment(docTag: JSDocTag) = {
    var next = docTag.getNextSibling
    val tokens = new ListBuffer[PsiElement]
    while (next != null && (
      next.isInstanceOf[LeafPsiElement] ||
        next.isInstanceOf[PsiWhiteSpace]
      )) {
      tokens.append(next)
      next = next.getNextSibling
    }
    val expr = tokens.map(t => t.getText).mkString("")
      .replaceAll("""\n\s*\* """, "\n")
      .replaceAll("""\*\/$""", "")
      .replaceAll(""".*?=\s*""", "= ")
    expr
  }

  private def findVarDecl(caretPsi: PsiElement, varName: String): GenTraversableOnce[JSType] = {
    var scope: Option[PsiElement] = None
    var stmts: GenTraversableOnce[PsiElement] = Iterator.empty
    val funcOpt = DeepJsLang.findParent[JSBlockStatement](caretPsi)
    val fileOpt = DeepJsLang.findParent[JSFile](caretPsi)
    if (funcOpt.isDefined) {
      scope = funcOpt
      stmts = funcOpt.get.getStatements.itr()
    } else if (fileOpt.isDefined) {
      scope = fileOpt
      stmts = fileOpt.get.getStatements.itr()
        .flatMap(cast[JSStatement](_))
    }
    val types = scope.itr().flatMap(b => stmts.itr()
      .flatMap(st => st match {
        case varSt: JSVarStatement =>
          varSt.getDeclarations
            .filter(own => varName.equals(own.getName))
            .map(own => own.getInitializer).itr()
            .flatMap(expr => ctx.findExprType(expr))
        case func: JSFunctionDeclaration =>
          if (varName.equals(func.getName)) {
            val rts = MainRes.getReturns(func)
              .flatMap(ret => ctx.findExprType(ret))
            val rt = JSDeepMultiType(rts.mem())
            Some(new JSFunctionTypeImpl(JSTypeSource.EMPTY, new util.ArrayList, rt))
          } else {
            None
          }
        case _ => None
      })
      .++(findVarDecl(b, varName))
    )
    types
  }

  def parseDocExpr(caretPsi: PsiElement, expr: String): GenTraversableOnce[JSType] = {
    frs(
      """^\s*=\s*(\w+)(\([^\)]*\)|)\s*$""".r.findFirstMatchIn(expr)
        .itr()
        .flatMap(found => {
          val varName = found.group(1)
          val isFuncCall = !found.group(2).equals("")
          findVarDecl(caretPsi, varName).itr()
            .flatMap(t => if (isFuncCall) Mt.getReturnType(t, ctx.subCtxEmpty()) else Some(t))
        })
      ,
      """^\s*=\s*require\('([^']+)'\)(\([^\)]*\)|)\s*$""".r.findFirstMatchIn(expr)
        .itr()
        .flatMap(found => {
          val path = found.group(1)
          val isFuncCall = !found.group(2).equals("")
          nit(caretPsi.getContainingFile)
            .flatMap(f => PathStrGoToDecl.getReferencedFileStrict(path, f)).itr
            .flatMap(file => List()
              ++ resolveCommonJsFormatDef(file)
              ++ resolveRequireJsFormatDef(file)
            ).lift(0).itr
            .flatMap(t => if (isFuncCall) Mt.getReturnType(t, ctx.subCtxEmpty()) else Some(t))
        })
      ,
      """^\s*=\s*([\s\S]+)$""".r.findFirstMatchIn(expr)
        .itr
        .flatMap(found => {
          // making it async, because IDEA does not recognize await keyword otherwise if any
          val expr = "(async () => (" + found.group(1) + "))()"
          val psiFile = PsiFileFactory.getInstance(caretPsi.getProject)
            .createFileFromText(JavascriptLanguage.INSTANCE, expr)
          nit(psiFile.getFirstChild)
            .flatMap(cast[JSExpressionStatementImpl](_))
            .flatMap(st => Option(st.getExpression))
            .flatMap(expr => ctx.subCtxEmpty().findExprType(expr))
            .flatMap(promiset => Mt.unwrapPromise(promiset)) // since we wrapped it in async
        })
    )
  }

  private def getArgDocExprType(func: JSFunction, para: JSParameterListElement): GenTraversableOnce[JSType] = {
    Option(JSDocumentationUtils.findDocComment(para))
      .flatMap(cast[JSDocCommentImpl](_)).itr
      .flatMap(tag => tag.getTags)
      .filter(tag => "param".equals(tag.getName))
      .filter(tag => Option(tag.getDocCommentData)
        .exists(data => Objects.equals(para.getName, data.getText)))
      .map(tag => getDocTagComment(tag))
      .flatMap(expr => parseDocExpr(para, expr))
  }

  def resolve(para: JSParameterListElement): GenTraversableOnce[JSType] = {
    val types = Option(para.getDeclaringFunction)
      .itr.flatMap(func => List[JSType]()
      ++ getArgDocExprType(func, para)
      ++ getCtxArgType(func, para)
      ++ getInlineFuncArgType(func, func.getParameters.indexOf(para))
      ++ getPrivateFuncArgType(func, func.getParameters.indexOf(para))
    )
    types
  }
}
