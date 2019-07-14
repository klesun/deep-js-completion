package org.klesun.deep_js_completion.completion_providers

import java.awt.Color
import java.util

import com.intellij.codeInsight.completion.{CompletionParameters, CompletionProvider, CompletionResultSet, PrioritizedLookupElement}
import com.intellij.codeInsight.lookup.{LookupElement, LookupElementBuilder}
import com.intellij.codeInsight.navigation.actions.GotoDeclarationHandler
import com.intellij.lang.ecmascript6.ES6FileType
import com.intellij.lang.javascript.JavaScriptFileType
import com.intellij.lang.javascript.psi.JSRecordType.{IndexSignature, PropertySignature}
import com.intellij.lang.javascript.psi.JSType.TypeTextFormat
import com.intellij.lang.javascript.psi.types.JSRecordMemberSourceFactory.EmptyMemberSource
import com.intellij.lang.javascript.psi.types.JSRecordTypeImpl.PropertySignatureImpl
import com.intellij.lang.javascript.psi.types._
import com.intellij.lang.javascript.psi.types.primitives.{JSBooleanType, JSNumberType, JSStringType}
import com.intellij.lang.javascript.psi.{JSExpression, JSFunction, JSProperty, JSRecordType, JSReferenceExpression, JSType}
import com.intellij.lang.javascript.settings.JSRootConfiguration
import com.intellij.openapi.actionSystem.DataContext
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi.{PsiElement, PsiFile}
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.ui.JBColor
import com.intellij.util.ProcessingContext
import javax.swing.ImageIcon
import org.klesun.deep_js_completion.completion_providers.PropNamePvdr._
import org.klesun.deep_js_completion.contexts.{ExprCtx, FuncCtx, SearchCtx}
import org.klesun.deep_js_completion.helpers.Mt
import org.klesun.deep_js_completion.structures.JSDeepFunctionTypeImpl
import org.klesun.lang.DeepJsLang._

import scala.collection.JavaConverters._
import scala.collection.mutable

object PropNamePvdr {
//  val imgURL = getClass.getResource("../icons/deep_16.png")
  val imgURL = getClass.getResource("../icons/deep_16_ruby2.png")
  val icon = new ImageIcon(imgURL)
  val DEEP_PRIO = 2000
  val PRIM_PRIO = 20
  val PROTO_PRIO = -100000

  def getIcon = icon

  private def getMaxDepth(isAutoPopup: Boolean) = {
    if (isAutoPopup) 35 else 120
  }

  private def formatKeyPsiValue(name: String, psi: PsiElement) = {
    cast[JSProperty](psi)
      .flatMap(prop => Option(prop.getValue))
      .map(v => v.getText)
      .filter(str => !str.equals(name))
  }

  private def makeLookup(propRec: PropRec, i: Int) = {
    val prop = propRec.prop
    val isBuiltIn = propRec.isBuiltIn
    var typeStr = Option(prop.getType)
      .map {
        case lit: JSPrimitiveLiteralType[_] =>
          val strVal = lit.getLiteral + ""
          if (strVal.trim equals "") lit.getTypeText(TypeTextFormat.PRESENTABLE) else "'" + strVal + "'"
        // convert IndexSignature back to PropertySignature when possible for nicer value preview
        case rect: JSRecordType =>
          new JSRecordTypeImpl(JSTypeSource.EMPTY, rect.getTypeMembers.asScala.map {
            case idx: IndexSignature => idx.getMemberParameterType match {
              case idxLit: JSStringLiteralTypeImpl => new PropertySignatureImpl(idxLit.getLiteral, idx.getMemberType, false, new EmptyMemberSource)
              case other => idx
            }
            case mem => mem
          }.asJava).getTypeText(TypeTextFormat.PRESENTABLE)
        case t => t.getTypeText(TypeTextFormat.PRESENTABLE)
      }
      .getOrElse("?")

    val name = prop.getMemberName
    val source = prop.getMemberSource
    val sourceElement = Option(source.getSingleElement).getOrElse(name)
    if (typeStr.equals("?") || typeStr.equals("*|*|*")) {
      val psiPreview = propRec.psi.flatMap(psi => formatKeyPsiValue(name, psi))
      if (psiPreview.nonEmpty) {
        typeStr = psiPreview.get
      }
    }

    var lookup = LookupElementBuilder.create(sourceElement, name)
      .withBoldness(!isBuiltIn)
      .withItemTextItalic(isBuiltIn)
      .withIcon(getIcon)
      .withTypeText(typeStr, true)
    var priority = DEEP_PRIO - i
    if (isBuiltIn) {
      val jbColor = new JBColor(new Color(90, 90, 90), new Color(150, 150, 150))
      lookup = lookup.withItemTextForeground(jbColor)
      priority = PRIM_PRIO
    }
    PrioritizedLookupElement.withPriority(lookup, priority)
  }

  def getNamedProps(typ: JSType, project: Project): It[PropRec] = {
    val mems = Mt.getProps(typ, project)
    mems.itr()
      .flatMap(idx => Mt
        .getAnyLiteralValues(idx.getMemberParameterType)
        .map(name => new PropertySignatureImpl(name, idx.getMemberType, false, new EmptyMemberSource))
        .filter(prop => !prop.getMemberName.startsWith("[Symbol."))
        .map(p => PropRec(p, isBuiltIn(typ), idx.psi)))
  }

  private def isBuiltIn(t: JSType): Boolean = {
    t.isInstanceOf[JSPrimitiveLiteralType[Any]] ||
      t.isInstanceOf[JSStringType] ||
      t.isInstanceOf[JSNumberType] ||
      t.isInstanceOf[JSBooleanType] ||
      t.isInstanceOf[JSFunction] ||
      t.isInstanceOf[JSDeepFunctionTypeImpl] ||
      t.isInstanceOf[JSArrayType] ||
      t.isInstanceOf[JSTupleType]
  }

  private def resolveMems(qual: JSExpression, parameters: CompletionParameters): It[PropRec] = {
    val depth = getMaxDepth(parameters.isAutoPopup)
    val search = new SearchCtx(depth, project=Some(qual.getProject))

    val funcCtx = FuncCtx(search)
    val exprCtx = ExprCtx(funcCtx, qual, 0)

    val tit = exprCtx.findExprType(qual).itr()
      .flatMap(typ => getNamedProps(typ, qual.getProject))
    tit.hasNext
    Console.println("checked first member in " + search.expressionsResolved + " exprs")
    tit
  }

  def getQualifier(offset: Int, psiFile: PsiFile): Option[JSExpression] = {
    // getPosition() returns element in a _fake_ PSI file with "IntellijIdeaRulezzz " (mind the space in the end) added after the
    // со caret - this may corrupt the PSI tree and give different count of arguments in a function call for example, so no using it!
    //val nullPsi = parameters.getPosition
    Option(psiFile)
      // original PSI points to different elements in `obj.<>prop,`, `obj.prop<>,` and `obj.<>,`, but
      // in all these cases previous PSI is a leaf of the reference expression we want to resolve
      .flatMap(f => Option(PsiTreeUtil.findElementOfClassAtOffset(f, offset - 1, classOf[JSReferenceExpression], false)))
      .flatMap(ref => Option(ref.getQualifier))
      .filter(qual => {
        // filter out cases when caret is _inside_ the qualifier - caret should always be to the right
        val qualEnd = qual.getTextOffset + qual.getTextLength
        qualEnd < offset
      })
      .filter(qual =>
          // do not provide completion for them since they
          // are trashed by deprecated Microsoft functions
          !"document".equals(qual.getText) &&
          !"window".equals(qual.getText))
  }

  private def getQualifier(parameters: CompletionParameters): Option[JSExpression] = {
    Option(parameters.getOriginalPosition)
      .flatMap(psi => getQualifier(psi.getTextOffset, parameters.getOriginalFile))
  }

  /**
   * the idea is to first show _typed_ options provided by WS, then hang for however
   * long we like to resolve our deep keys, and only then to show the useless
   * _anything_ options you would see if "Only Type-Based Completion" setting is off
   */
  private def processBuiltIns(
    parameters: CompletionParameters,
    result: CompletionResultSet,
    jsConfig: JSRootConfiguration,
  ) = {
    var onlyTyped = if (jsConfig != null) jsConfig.isOnlyTypeBasedCompletion else false
    val suggested = new mutable.HashSet[String]()

    val builtIns = new util.ArrayList[LookupElement]()
    result.runRemainingContributors(parameters, otherSourceResult => {
      builtIns.add(otherSourceResult.getLookupElement)
    })

    var builtInsLeft = new util.ArrayList[LookupElement]()
    val guessBuiltIns = new util.ArrayList[LookupElement]()
    val showOption = (lookup: LookupElement) => {
      result.addElement(lookup)
      suggested.add(lookup.getLookupString)
    }

    builtIns.forEach(lookupArg => {
      var lookup = lookupArg
      val protos = List("constructor", "hasOwnProperty", "isPrototypeOf",
        "propertyIsEnumerable", "toLocaleString", "toString", "valueOf")
      // 99.0 (group 94) - inferred type property completion
      // 5.0 (group 6) - property completion guessed from usage
      val isGuess = builtIns.size() > 60 ||
        cast[PrioritizedLookupElement[LookupElement]](lookup)
          .forall(pri => pri.getPriority < 99.0 || protos.contains(pri.getLookupString))

      lookup = cast[PrioritizedLookupElement[LookupElement]](lookup)
        .filter(prio => protos.contains(prio.getLookupString))
        .map(prio => prio.getDelegate)
        .map(dele => PrioritizedLookupElement.withPriority(dele, PROTO_PRIO))
        .getOrElse(lookup)
      if (isGuess) {
        guessBuiltIns.add(lookup)
      } else if (!onlyTyped) {
        builtInsLeft.add(lookup)
      } else {
        showOption(lookup)
      }
    })
    if (!onlyTyped && builtInsLeft.size() < 30) {
      // if WS knows for sure what type var has, it will show only useful
      // suggestions even if "Only Type-Based Completion" is set to false
      builtInsLeft.forEach(b => showOption(b))
      builtInsLeft = new util.ArrayList[LookupElement]()
    }
    (suggested, builtInsLeft.asScala ++ guessBuiltIns.asScala)
  }

//  private def printExprTree(root: ExprCtx, depth: Int): Unit = {
//    val indent = " " * depth
//    Console.println(indent + SearchCtx.formatPsi(root.expr))
//    for (subCtx <- root.children.asScala) {
//      printExprTree(subCtx, depth + 1)
//    }
//  }
}

case class PropRec(
  prop: PropertySignature,
  isBuiltIn: Boolean,
  psi: Option[PsiElement],
)

class PropNamePvdr extends CompletionProvider[CompletionParameters] with GotoDeclarationHandler {
  override def addCompletions(
    parameters: CompletionParameters,
    context: ProcessingContext,
    result: CompletionResultSet
  ) {
    if (!parameters.getOriginalFile.getFileType.equals(JavaScriptFileType.INSTANCE) &&
        !parameters.getOriginalFile.getFileType.equals(ES6FileType.INSTANCE)
    ) {
      // do not run this plugin in typescript, the language is
      // fully typed itself, no need for additional _deep_ typing
      return
    }

    val qualOpt = getQualifier(parameters)
    if (qualOpt.isEmpty) return
    val qual = qualOpt.get

    val jsConfig = JSRootConfiguration.getInstance(qual.getProject)
    val (suggested, builtInsLeft) = processBuiltIns(parameters, result, jsConfig)

    val startTime = System.nanoTime
    val mems = resolveMems(qual, parameters)

    val hasAny = mems.hasNext
    var elapsed = System.nanoTime - startTime
    if (hasAny) {
      result.addLookupAdvertisement("Resolved first key in " + (elapsed / 1000000000.0))
    } else {
      result.addLookupAdvertisement("No keys resolved in " + (elapsed / 1000000000.0))
    }
    val deepOptions = new util.ArrayList[String]()
    mems
      .zipWithIndex
      .map({case (e,i) => makeLookup(e,i)})
      .foreach(ourKup => {
        val asField = ourKup.getLookupString
        val asFunc = asField + "()"
        deepOptions.add(asField)
        if (!suggested.contains(asField) && !suggested.contains(asFunc)) {
          result.addElement(ourKup)
          suggested.add(asField)
          suggested.add(asFunc)
        }
      })

    elapsed = System.nanoTime - startTime
    val deepOptionsUnq = deepOptions.asScala.toList.distinct
    result.addLookupAdvertisement("Resolved all " + deepOptionsUnq.size + " in " + (elapsed / 1000000000.0) + ": " + deepOptionsUnq.slice(0, 7).mkString(",") + "...")

    // guessed built-in suggestions left
    builtInsLeft
      .foreach(builtInKup => {
        val memName = builtInKup.getLookupString
        if (!suggested.contains(memName)) {
          result.addElement(builtInKup)
        }
      })

    //if (SearchCtx.DEBUG_OBJ.PRINT_PSI_TREE) {
    //  Console.println("========= tree ========")
    //  printExprTree(exprCtx, 0)
    //}
  }

  override def getGotoDeclarationTargets(caretPsi: PsiElement, mouseOffset: Int, editor: Editor): Array[PsiElement] = {
    val depth = getMaxDepth(false)
    val search = new SearchCtx(depth, project=Option(editor.getProject))

    nit(caretPsi)
      .flatMap(leaf => Option(leaf.getParent))
      .flatMap(cast[JSReferenceExpression](_))
      .flatMap(ref => nit(ref.getQualifier)
        .flatMap(qual => search.findExprType(qual))
        .flatMap(typ => Mt.getProps(typ, caretPsi.getProject))
        .filter(prop => Mt.getAnyLiteralValues(prop.getMemberParameterType)
          .exists(lit => lit equals ref.getReferenceName)))
      .flatMap(p => p.psi)
      .itr().lift(0)
      .toArray
  }

  override def getActionText(dataContext: DataContext): String = null
}
