package org.klesun.deep_js_completion.resolvers.var_res

import java.util
import java.util.Objects

import com.intellij.lang.javascript.documentation.JSDocumentationUtils
import com.intellij.lang.javascript.psi._
import com.intellij.lang.javascript.psi.impl.JSReferenceExpressionImpl
import com.intellij.lang.javascript.psi.jsdoc.JSDocTag
import com.intellij.lang.javascript.psi.jsdoc.impl.JSDocCommentImpl
import com.intellij.lang.javascript.psi.types._
import com.intellij.psi.impl.source.tree.LeafPsiElement
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiElement, PsiFile, PsiWhiteSpace}
import org.klesun.deep_js_completion.contexts.{IExprCtx, IFuncCtx}
import org.klesun.deep_js_completion.entry.PathStrGoToDecl
import org.klesun.deep_js_completion.helpers.{Mkt, Mt}
import org.klesun.deep_js_completion.resolvers.var_res.ArgRes._
import org.klesun.deep_js_completion.resolvers.{MainRes, VarRes}
import org.klesun.deep_js_completion.structures.{EInstType, JSDeepModuleTypeImpl}
import org.klesun.lang.Lang
import org.klesun.lang.Lang.cast

import scala.collection.GenTraversableOnce
import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

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

  // for modules resolution - if module is a function - return
  // this function, otherwise wrap the object in a function
  private def ensureFunc(clsT: JSType): Option[JSType] = {
    val funcTs = Mt.flattenTypes(clsT).map(t => {t match {
      case funcT: JSFunctionTypeImpl => funcT
      case _ => new JSFunctionTypeImpl(JSTypeSource.EMPTY, List[JSParameterTypeDecorator]().asJava, clsT)
    }}: JSFunctionTypeImpl)
    Mt.mergeTypes(funcTs)
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
}

case class ArgRes(ctx: IExprCtx) {

  private def getInlineFuncArgType(func: JSFunction, argOrder: Integer): GenTraversableOnce[JSType] = {
    Option(func.getParent).toList
      .flatMap(cast[JSArgumentList](_))
      .flatMap(argList => Option(argList.getArguments.indexOf(func)).toList
        .flatMap(inlineFuncArgOrder => Option(true)
          .flatMap(ok => Option(argList.getParent))
          .flatMap(cast[JSCallExpression](_))
          .flatMap(call => Option(call.getMethodExpression))
          .flatMap(cast[JSReferenceExpressionImpl](_))
          .toList
          .flatMap(ref =>
            Option(ref.getQualifier)
              .filter(expr => argOrder == 0)
              .filter(expr => List("forEach", "map", "filter", "sort").contains(ref.getReferencedName))
              .flatMap(expr => ctx.findExprType(expr))
              .flatMap(arrt => Mt.getKey(arrt, None)).toList
              ++
            Option(ref.getQualifier)
              .filter(expr => argOrder == 1)
              .filter(expr => List("reduce").contains(ref.getReferencedName))
              .flatMap(expr => ctx.findExprType(expr))
              .flatMap(arrt => Mt.getKey(arrt, None)).toList
              ++
            Option(ref.getQualifier)
              // func arg order does not matter, it may be 0 or 1, maybe something else as well
              .filter(expr => List("use", "get", "post").contains(ref.getReferencedName))
              .flatMap(expr => ctx.findExprType(expr)).toList
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
              Option(ref.getQualifier)
                .filter(expr => argOrder == 0)
                .filter(expr => List("then").contains(ref.getReferencedName))
                .flatMap(expr => ctx.findExprType(expr)).toList
                .flatMap(promiset => Mt.unwrapPromise(promiset))
              ++
              // completion on arg of anonymous function based on what is passed to it
              // i should use _deep_ logic instead of ref.resolve() one day...
              Option(ref.resolve())
                .flatMap(cast[JSDefinitionExpression](_))
                .flatMap(callerDef => Option(callerDef.getParent))
                .flatMap(cast[JSAssignmentExpression](_))
                .flatMap(defi => Option(defi.getROperand))
                .flatMap(cast[JSFunction](_)).toList
                .flatMap(callerFunc => callerFunc.getParameters.lift(inlineFuncArgOrder)
                  .flatMap(callerArg => cast[JSParameter](callerArg))
                  .toList.flatMap(par => VarRes.findVarUsages(par, par.getName))
                  .flatMap(usage => Option(usage.getParent)
                    .flatMap(cast[JSCallExpression](_))
                    .filter(call => usage eq call.getMethodExpression))
                  .flatMap(call => call.getArguments.lift(argOrder))
                  .flatMap(value => ctx.subCtxEmpty().findExprType(value)))
          )
        )
      )
  }

  // private function completion (based on scanning current
  // file for usages and taking what is passed to the function)
  private def getPrivateFuncArgType(func: JSFunction, argOrder: Integer): GenTraversableOnce[JSType] = {
    Option(func.getParent).toList
      .flatMap(cast[JSVariable](_))
      .flatMap(vari => VarRes.findVarUsages(vari, vari.getName))
      .flatMap(usage => Option(usage.getParent)
        .flatMap(cast[JSCallExpression](_))
        .filter(call => usage eq call.getMethodExpression))
      .flatMap(call => call.getArguments.lift(argOrder))
      .flatMap(value => ctx.subCtxEmpty().findExprType(value))
  }

  private def getCtxArgType(func: JSFunction, para: JSParameterListElement): GenTraversableOnce[JSType] = {
    val order = func.getParameters.indexOf(para)
    grabClosureCtxs(ctx.func()).toList
      .filter(_.getClosurePsi().exists(_ equals func))
      .lift(0).toList
      .flatMap(_.getArg(order))
  }

  private def resolveKlesunWhenLoadedSupplierDef(file: PsiFile): Option[JSType] = {
    PsiTreeUtil.findChildrenOfType(file, classOf[JSAssignmentExpression]).asScala
      .filter(assi => assi.getText.startsWith("klesun.whenLoaded")).toList.lift(0)
      .flatMap(assi => Option(assi.getROperand)) // \(^o^)/
      .flatMap(moduleSupplier => ctx.findExprType(moduleSupplier))
  }

  private def resolveRequireJsSupplierDef(file: PsiFile): Option[JSType] = {
    PsiTreeUtil.findChildrenOfType(file, classOf[JSCallExpression]).asScala
      .filter(assi => assi.getText.startsWith("define(")).toList.lift(0)
      .flatMap(call => call.getArguments.lift(1))
      .flatMap(moduleSupplier => ctx.findExprType(moduleSupplier))
  }

  def resolveCommonJsFormatDef(file: PsiFile): Option[JSType] = {
    val types = file.getChildren
      .flatMap(cast[JSExpressionStatement](_))
      .flatMap(_.getChildren)
      .flatMap(cast[JSAssignmentExpression](_))
      .flatMap(ass => Option(ass.getROperand)
        .flatMap(value => Option(ass.getLOperand)
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
                .map(name => Mt.mkProp(name, () => ctx.findExprType(value), Some(vari)))
                .map(prop => new JSRecordTypeImpl(JSTypeSource.EMPTY, List(prop).asJava))
            } else {
              None
            }
          })
        )
      )
    Mt.mergeTypes(types)
  }

  private def resolveRequireJsFormatDef(file: PsiFile): Option[JSType] = {
    val types = List[JSType]() ++
      resolveKlesunWhenLoadedSupplierDef(file) ++
      resolveRequireJsSupplierDef(file)
    Mt.mergeTypes(types.flatMap(sup => Mt.getReturnType(sup, ctx.subCtxEmpty())))
  }

  private def getKlesunRequiresArgType(func: JSFunction): Option[JSType] = Option(func.getParent)
    .flatMap(cast[JSAssignmentExpression](_))
    .flatMap(assi => Option(assi.getDefinitionExpression))
    .flatMap(defi => Option(defi.getExpression))
    .flatMap(cast[JSReferenceExpressionImpl](_))
    .filter(ref => List("then").contains(ref.getReferencedName))
    .flatMap(ref => Option(ref.getQualifier))
    .flatMap(cast[JSCallExpression](_))
    .filter(call => Option(call.getMethodExpression)
      .map(e => e.getText).getOrElse("").equals("klesun.requires"))
    .flatMap(call => call.getArguments.toList.lift(0))
    .flatMap(arg => PathStrGoToDecl.getReferencedFile(arg))
    .flatMap(file => resolveRequireJsFormatDef(file))
    .flatMap(clsT => ensureFunc(clsT))

  private def getDocTagComment(docTag: JSDocTag) = {
    var next = docTag.getNextSibling
    val tokens = new ListBuffer[PsiElement]
    while (next != null && (
      next.isInstanceOf[LeafPsiElement] ||
        next.isInstanceOf[PsiWhiteSpace]
      )) {
      tokens.append(next)
      next = next.getNextSibling
    }
    tokens.map(t => t.getText).mkString("")
      .replaceAll("""\n\s*\* """, "\n")
      .replaceAll("""\*\/$""", "")
  }

  private def findVarDecl(caretPsi: PsiElement, varName: String): Option[JSType] = {
    var scope: Option[PsiElement] = None
    var stmts = List[JSStatement]()
    val funcOpt = Lang.findParent[JSBlockStatement](caretPsi)
    val fileOpt = Lang.findParent[JSFile](caretPsi)
    if (funcOpt.isDefined) {
      scope = funcOpt
      stmts = funcOpt.get.getStatements.toList
    } else if (fileOpt.isDefined) {
      scope = fileOpt
      stmts = fileOpt.get.getStatements.toList
        .flatMap(cast[JSStatement](_))
    }
    val types = scope.toList.flatMap(b => stmts
      .flatMap(st => st match {
        case varSt: JSVarStatement =>
          varSt.getDeclarations
            .filter(own => varName.equals(own.getName))
            .map(own => own.getInitializer)
            .flatMap(expr => ctx.findExprType(expr))
        case func: JSFunctionDeclaration =>
          if (varName.equals(func.getName)) {
            val rts = MainRes.getReturns(func)
              .flatMap(ret => ctx.findExprType(ret))
            val rt = Mt.mergeTypes(rts).getOrElse(JSUnknownType.JS_INSTANCE)
            Some(new JSFunctionTypeImpl(JSTypeSource.EMPTY, new util.ArrayList, rt))
          } else {
            None
          }
        case _ => None
      })
      .++(findVarDecl(b, varName))
    )
    Mt.mergeTypes(types)
  }

  private def parseDocExpr(caretPsi: PsiElement, expr: String): Iterable[JSType] = {
    Option(null)
      .orElse("""^\s*=\s*(\w+)(\([^\)]*\)|)\s*$""".r.findFirstMatchIn(expr)
        .flatMap(found => {
          val varName = found.group(1)
          val isFuncCall = !found.group(2).equals("")
          findVarDecl(caretPsi, varName)
            .flatMap(t => if (isFuncCall) Mt.getReturnType(t, ctx.subCtxEmpty()) else Some(t))
        }))
      .orElse("""^\s*=\s*from\('([^']+)'\)(\([^\)]*\)|)\s*$""".r.findFirstMatchIn(expr)
        .flatMap(found => {
          val path = found.group(1)
          val isFuncCall = !found.group(2).equals("")
          Option(caretPsi.getContainingFile)
            .flatMap(f => PathStrGoToDecl.getReferencedFile(path, f)).toList
            .flatMap(file => List()
              ++ resolveCommonJsFormatDef(file)
              ++ resolveRequireJsFormatDef(file)
            ).lift(0)
            .flatMap(t => if (isFuncCall) Mt.getReturnType(t, ctx.subCtxEmpty()) else Some(t))
        }))
  }

  private def getArgDocExprType(func: JSFunction, para: JSParameterListElement): List[JSType] = {
    Option(JSDocumentationUtils.findDocComment(para))
      .flatMap(cast[JSDocCommentImpl](_)).toList
      .flatMap(tag => tag.getTags)
      .filter(tag => "param".equals(tag.getName))
      .filter(tag => Option(tag.getDocCommentData)
        .exists(data => Objects.equals(para.getName, data.getText)))
      .map(tag => getDocTagComment(tag))
      .flatMap(expr => parseDocExpr(para, expr))
  }

  def resolve(para: JSParameterListElement): Option[JSType] = {
    val types = Option(para.getDeclaringFunction)
      .toList.flatMap(func => List[JSType]()
      ++ getArgDocExprType(func, para)
      ++ getCtxArgType(func, para)
      ++ getInlineFuncArgType(func, func.getParameters.indexOf(para))
      ++ getPrivateFuncArgType(func, func.getParameters.indexOf(para))
      ++ getKlesunRequiresArgType(func))
    Mt.mergeTypes(types)
  }
}
