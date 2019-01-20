package org.klesun.deep_js_completion.resolvers

import java.util
import java.util.Objects

import com.intellij.lang.javascript.psi.JSRecordType.TypeMember
import com.intellij.lang.javascript.psi._
import com.intellij.lang.javascript.psi.ecma6._
import com.intellij.lang.javascript.psi.impl.JSDestructuringParameterImpl
import com.intellij.lang.javascript.psi.resolve.JSScopeNamesCache
import com.intellij.lang.javascript.psi.types._
import com.intellij.psi.{PsiElement, PsiFile}
import org.klesun.deep_js_completion.contexts.IExprCtx
import org.klesun.deep_js_completion.entry.PathStrGoToDecl
import org.klesun.deep_js_completion.helpers.Mt
import org.klesun.deep_js_completion.resolvers.VarRes._
import org.klesun.deep_js_completion.resolvers.var_res.ArgRes
import org.klesun.deep_js_completion.structures.{DeepIndexSignatureImpl, JSDeepFunctionTypeImpl}
import org.klesun.lang.Lang
import org.klesun.lang.Lang._

import scala.collection.GenTraversableOnce
import scala.collection.JavaConverters._

object VarRes {

  def findVarAt(file: PsiFile, name: String): GenTraversableOnce[JSVariable] = {
    JSScopeNamesCache.findNamedElementsInStubScope(name, file).asScala
      .flatMap(cast[JSVariable](_))
  }

  def findVarUsages(decl: PsiElement, name: String): List[JSReferenceExpression] = {
    // maybe this could be used instead: JSScopeNamesUsages
    if (Option(decl.getContainingFile).forall(f => f.getName.endsWith(".d.ts"))) {
      List()
    } else {
      val t1 = System.nanoTime
      val scope: PsiElement = Lang.findParent[JSFunctionExpression](decl)
        .getOrElse(decl.getContainingFile)
      val result = Lang.findChildren[JSReferenceExpression](scope)
        .filter(usage => Objects.equals(usage.getReferenceName, name))
        .filter(usage => !Objects.equals(usage, decl))
        .filter(usage => Objects.equals(decl, usage.resolve()))
      val duration = (System.nanoTime - t1) / 1e9d
      result
    }
  }

  private def findRefUsages(ref: JSReferenceExpression): List[JSReferenceExpression] = {
    Option(ref.resolve()).toList.flatMap(decl => findVarUsages(decl, ref.getReferenceName))
      .filter(usage => !usage.equals(ref))
  }
}

/**
 * resolves variable type
 */
case class VarRes(ctx: IExprCtx) {

  private def first[T](suppliers: (() => Option[T])*): Option[T] = {
    suppliers.iterator.flatMap(s => s())
      .take(1).toList.lift(0)
  }

  private def resolveAssignmentTo(usage: JSExpression): GenTraversableOnce[JSType] = {
    Option(usage.getParent).toList.flatMap {
      case superRef: JSReferenceExpression => first(() => None
        // someVar.push(value)
        , () => Option(superRef.getReferenceName)
          .filter(refName => List("push", "unshift").contains(refName))
          .flatMap(refName => Option(superRef.getParent))
          .flatMap(cast[JSCallExpression](_))
          .flatMap(call => call.getArguments.lift(0))
          .flatMap(value => ctx.findExprType(value))
          .map(elT => new JSArrayTypeImpl(elT, JSTypeSource.EMPTY))
        // someVar.someKey = 123
        , () => Mt.mergeTypes(Option(superRef.getReferenceName).toList
          .flatMap(name => resolveAssignmentTo(superRef).toList
            .map(valt => Mt.mkProp(name, () => Some(valt), Some(superRef)))
            .map((prop: TypeMember) => new JSRecordTypeImpl(JSTypeSource.EMPTY, List(prop).asJava))))
      )
      // someVar[i] = value
      case indexing: JSIndexedPropertyAccessExpression =>
        Option(indexing.getQualifier)
          .filter(qual => usage equals qual).toList
          .flatMap(qual => resolveAssignmentTo(indexing))
          .map(valT => {
            val keyt = ctx.findExprType(indexing.getIndexExpression).orNull
            DeepIndexSignatureImpl(keyt, valT, Some(indexing))
          })
          .map((prop: TypeMember) => new JSRecordTypeImpl(JSTypeSource.EMPTY, List(prop).asJava))
      // var someVar = null;
      // someVar = initializeSomething()
      case usage: JSDefinitionExpression =>
        Option(usage.getParent)
          .flatMap(cast[JSAssignmentExpression](_))
          .flatMap(defi => Option(defi.getROperand))
          .flatMap(expr => ctx.findExprType(expr))
      case _ => List[JSType]()
    }: GenTraversableOnce[JSType]
  }

  private def parseTypePsi(typePsi: JSTypeDeclaration, generics: Map[String, () => Array[JSType]]): GenTraversableOnce[JSType] = {
    typePsi match {
      case arrts: TypeScriptArrayType =>
        val elts = Option(arrts.getType).toList
          .flatMap(eltPsi => parseTypePsi(eltPsi, generics))
        val arrt = new JSArrayTypeImpl(Mt.mergeTypes(elts).orNull, JSTypeSource.EMPTY)
        Some(arrt)
      case sints: TypeScriptSingleType =>
        val fqn = sints.getQualifiedTypeName
        if (generics.contains(fqn)) {
          generics(fqn).apply()
        } else {
          val clsType = JSTypeUtils.createType(sints.getQualifiedTypeName, JSTypeSource.EMPTY)
          val clsGenerics: java.util.List[JSType] = sints.getTypeArguments.map(
            gena => Mt.mergeTypes(parseTypePsi(gena, generics))
              .getOrElse(JSUnknownType.JS_INSTANCE)
          ).toList.asJava
          Some(new JSGenericTypeImpl(JSTypeSource.EMPTY, clsType, clsGenerics))
        }
      case _ => None
    }
  }

  private def getGenericTypeFromArg(typePsi: JSTypeDeclaration, getArgt: () => GenTraversableOnce[JSType], generic: String): GenTraversableOnce[JSType] = {
    typePsi match {
      case union: TypeScriptUnionOrIntersectionType =>
        union.getTypes.flatMap(subTypePsi =>
          getGenericTypeFromArg(subTypePsi, getArgt, generic))
      case obj: TypeScriptObjectType =>
        val getSubType = () => getArgt().toList.flatMap(t => Mt.getKey(t, None))
        obj.getIndexSignatures.flatMap(sig => getGenericTypeFromArg(sig.getType, getSubType, generic))
      case sints: TypeScriptSingleType =>
        val fqn = sints.getQualifiedTypeName
        if (generic equals fqn) {
          getArgt()
        } else {
          None
        }
      case _ => None
    }
  }

  private def resolveDtsFunc(tsFunc: TypeScriptFunctionSignature): GenTraversableOnce[JSType] = {
    val genericPsis = tsFunc.getTypeParameters
    val args = tsFunc.getParameters
    val rtPsiOpt = Option(tsFunc.getReturnTypeElement)
    rtPsiOpt.map(rtPsi => new JSDeepFunctionTypeImpl(tsFunc, ctx.subCtxEmpty().func(), callCtx => {
      val generics: Map[String, () => Array[JSType]] = genericPsis
        .flatMap(psi => Option(psi.getName))
        .map(generic => generic -> (() => {
          args.zipWithIndex.flatMap({case (argPsi, i) => Option(argPsi.getTypeElement)
            .flatMap(cast[TypeScriptType](_))
            .toList.flatMap(tst => getGenericTypeFromArg(
              tst, () => callCtx.func().getArg(i), generic)
            )})
        })).toMap
      parseTypePsi(rtPsi, generics)
    }))
  }

  private def resolveDestructEl(el: PsiElement): GenTraversableOnce[JSType] = {
    el match {
      // let doStuff = ({a, b}) => {...};
      case para: JSDestructuringParameterImpl => ArgRes(ctx).resolve(para)
      // let {a, b} = getObj();
      case obj: JSDestructuringElement => Option(obj.getInitializer)
        .flatMap(qual => ctx.findExprType(qual)).toList
      case _ => None
    }
  }

  private def resolveMainDeclVar(dest: JSVariable): GenTraversableOnce[JSType] = {
    Option(dest.getInitializer)
      .flatMap(expr => ctx.findExprType(expr)) ++
    Option(dest.getParent).flatMap {
      case prop: JSDestructuringShorthandedProperty =>
        val types = Option(prop.getParent)
          .flatMap(cast[JSDestructuringObject](_))
          .flatMap(obj => Option(obj.getParent)).toList
          .flatMap(resolveDestructEl)
          .flatMap(qualT => {
            val keyTOpt = Option(dest.getName)
              .map(name => new JSStringLiteralTypeImpl(name, true, JSTypeSource.EMPTY))
            Mt.getKey(qualT, keyTOpt)
          })
        Mt.mergeTypes(types)
      case arr: JSDestructuringArray =>
        val types = Option(arr.getParent).toList
          .flatMap(el => resolveDestructEl(el))
          .flatMap(qualT => {
            val keyTOpt = Option(arr.getElements.indexOf(dest))
              .filter(idx => idx > -1)
              .map(idx => new JSStringLiteralTypeImpl(idx + "", true, JSTypeSource.EMPTY))
            Mt.getKey(qualT, keyTOpt)
          })
        Mt.mergeTypes(types)
      case varst: JSVarStatement =>
        Option(varst.getParent)
          .flatMap(cast[JSForInStatement](_))
          .flatMap(st => Option(st.getCollectionExpression))
          .flatMap(arrexpr => ctx.findExprType(arrexpr))
          .flatMap(arrt => Mt.getKey(arrt, None))
      case _ => None
    }
  }

  // may be defined in a different file unlike resolveAssignment()
  private def resolveFromMainDecl(psi: PsiElement): GenTraversableOnce[JSType] = {
    psi match {
      case para: JSParameter => ArgRes(ctx).resolve(para) ++ resolveMainDeclVar(para)
      case dest: JSVariable => resolveMainDeclVar(dest)
      case prop: JSProperty => Option(prop.getValue)
        .flatMap(expr => ctx.findExprType(expr))
      case prop: JSDefinitionExpression => Option(prop.getExpression)
        .flatMap(expr => ctx.findExprType(expr))
      case tsFunc: TypeScriptFunctionSignature => {
        if (("then" equals tsFunc.getName) &&
            List("lib.es2015.promise.d.ts", "lib.es5.d.ts").contains(tsFunc.getContainingFile.getName)
        ) {
          // es2015 d.ts has some weird return type - Promise<TResult1 | TResult2>,
          // it results in irrelevant options, so I'm overriding it here
          None
        } else {
          resolveDtsFunc(tsFunc)
        }
      }
      case func: JSFunction => Mt.mergeTypes(MainRes.getReturns(func)
        .flatMap(expr => ctx.findExprType(expr))
        .map(rett => new JSFunctionTypeImpl(JSTypeSource.EMPTY, new util.ArrayList[JSParameterTypeDecorator](), rett)))
      case _ =>
        //println("Unsupported var declaration - " + psi.getClass + " " + psi.getText)
        None
    }
  }

  private def getDeclarations(ref: JSReferenceExpression): GenTraversableOnce[PsiElement] = {
    val isProp = ref.getQualifier != null
    // it would be nice to always use es2018 instead of es2015 somehow
    val psis = Option(ref.resolve()).toList
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
      .flatMap(call => call.getArguments.lift(0)).toList
      .flatMap(PathStrGoToDecl.getReferencedFile)
  }

  def resolve(ref: JSReferenceExpression): Option[JSType] = {
    Mt.mergeTypes(
      Option(ref.getQualifier)
        .flatMap(qual => ctx.findExprType(qual))
        .flatMap(qualT => {
          val keyTOpt = Option(ref.getReferenceName)
            .map(name => new JSStringLiteralTypeImpl(name, true, JSTypeSource.EMPTY))
          val result = Mt.getKey(qualT, keyTOpt)
          result
        })
      ++
      Option(ref.getReferenceName).toList
        .flatMap(varName => Option(ref.getQualifier).toList
          .flatMap(assertAtModuleEpxr)
          .flatMap(file => findVarAt(file, varName))
          .flatMap(vari => resolveMainDeclVar(vari)))
      ++
      findRefUsages(ref)
        .flatMap(usage => resolveAssignmentTo(usage))
      ++
      getDeclarations(ref).toList
        .flatMap(psi => resolveFromMainDecl(psi))
    )
  }
}
