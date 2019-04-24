package org.klesun.deep_js_completion.completion_providers

import java.util

import com.intellij.codeInsight.completion.{CompletionParameters, CompletionProvider, CompletionResultSet, PrioritizedLookupElement}
import com.intellij.codeInsight.lookup.{LookupElement, LookupElementBuilder}
import com.intellij.codeInsight.navigation.actions.GotoDeclarationHandler
import com.intellij.lang.javascript.psi.JSRecordType.{IndexSignature, PropertySignature}
import com.intellij.lang.javascript.psi.JSType.TypeTextFormat
import com.intellij.lang.javascript.psi.types.JSRecordMemberSourceFactory.EmptyMemberSource
import com.intellij.lang.javascript.psi.types.JSRecordTypeImpl.PropertySignatureImpl
import com.intellij.lang.javascript.psi.types._
import com.intellij.lang.javascript.psi.types.primitives.{JSBooleanType, JSNumberType, JSStringType}
import com.intellij.lang.javascript.psi.{JSFunction, JSRecordType, JSReferenceExpression, JSType}
import com.intellij.lang.javascript.settings.JSRootConfiguration
import com.intellij.openapi.actionSystem.DataContext
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.ui.JBColor
import com.intellij.util.ProcessingContext
import javax.swing.ImageIcon
import org.klesun.deep_js_completion.completion_providers.PropNamePvdr.{getNamedProps, _}
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

  def getIcon = icon

  private def getMaxDepth(isAutoPopup: Boolean) = {
    if (isAutoPopup) 25 else 40
  }

  private def makeLookup(propRec: PropRec, i: Int) = {
    val prop = propRec.prop
    val isBuiltIn = propRec.isBuiltIn
    val typeStr = Option(prop.getType)
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

    var lookup = LookupElementBuilder.create(sourceElement, name)
      .withBoldness(!isBuiltIn)
      .withItemTextItalic(isBuiltIn)
      .withIcon(getIcon)
      .withTypeText(typeStr, true)
    var priority = DEEP_PRIO - i
    if (isBuiltIn) {
      lookup = lookup.withItemTextForeground(JBColor.GRAY)
      priority = 0
    }
    PrioritizedLookupElement.withPriority(lookup, priority)
  }

  def getNamedProps(typ: JSType, project: Project): It[PropertySignature] = {
    val mems = Mt.getProps(typ, project)
    mems.itr().flatMap(idx => Mt
        .getAnyLiteralValues(idx.getMemberParameterType)
        .map(name => new PropertySignatureImpl(name, idx.getMemberType, false, new EmptyMemberSource))
        .filter(prop => !prop.getMemberName.startsWith("[Symbol.")))
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
)

class PropNamePvdr extends CompletionProvider[CompletionParameters] with GotoDeclarationHandler {
  override def addCompletions(
    parameters: CompletionParameters,
    context: ProcessingContext,
    result: CompletionResultSet
  ) {
    // getPosition() returns element in a _fake_ PSI file with "IntellijIdeaRulezzz " (mind the space in the end) added after the
    // со caret - this may corrupt the PSI tree and give different count of arguments in a function call for example, so no using it!
    //val nullPsi = parameters.getPosition
    val nullPsi = Option(parameters.getOriginalPosition)
      .flatMap(leaf => Option(parameters.getOriginalFile)
        // original PSI points to different elements in `obj.<>prop,`, `obj.prop<>,` and `obj.<>,`, but
        // in all these cases previous PSI is a leaf of the reference expression we want to resolve
        .flatMap(f => Option(PsiTreeUtil.findElementOfClassAtOffset(f, leaf.getTextOffset - 1, classOf[JSReferenceExpression], false))))
      .orNull
    val project = if (nullPsi != null) nullPsi.getProject else null
    val jsConfig = if (project != null) JSRootConfiguration.getInstance(project) else null
    val onlyTyped = if (jsConfig != null) jsConfig.isOnlyTypeBasedCompletion else false

    def getBuiltIns(): util.ArrayList[(Boolean, LookupElement)] = {
      val builtInSuggestions = new util.ArrayList[(Boolean, LookupElement)]
      result.runRemainingContributors(parameters, otherSourceResult => {
        var lookup = otherSourceResult.getLookupElement
        // 99.0 (group 94) - inferred type property completion
        // 5.0 (group 6) - property completion guessed from usage
        val isGuess = cast[PrioritizedLookupElement[LookupElement]](lookup)
          .forall(pri => pri.getPriority < 99.0)

        val protos = List("constructor", "hasOwnProperty", "isPrototypeOf",
          "propertyIsEnumerable", "toLocaleString", "toString", "valueOf")
        lookup = cast[PrioritizedLookupElement[LookupElement]](lookup)
          .filter(prio => protos.contains(prio.getLookupString))
          .map(prio => prio.getDelegate)
          .map(dele => PrioritizedLookupElement.withPriority(dele, DEEP_PRIO - 198))
          .getOrElse(lookup)
        builtInSuggestions.add((isGuess || !onlyTyped, lookup))
      })
      builtInSuggestions
    }

    def addsTypeInfo(ourKup: LookupElement, builtIns: util.ArrayList[(Boolean, LookupElement)]): Boolean = {
      !builtIns.asScala.exists((tuple) => {
        val (isGuess, builtInKup) = tuple
        var memName = builtInKup.getLookupString
        if (memName.endsWith("()")) {
          memName = substr(memName, 0, -2)
        }
        val isBuiltInUseful = !isGuess && onlyTyped &&
          (memName equals( ourKup.getLookupString))
        isBuiltInUseful
      })
    }
    val depth = getMaxDepth(parameters.isAutoPopup)
    val search = new SearchCtx(depth, project=Option(parameters.getEditor.getProject))
    val funcCtx = FuncCtx(search)
    val exprCtx = ExprCtx(funcCtx, nullPsi, 0)

    val startTime = System.nanoTime
    var typesGot = 0
    var propsGot = 0

    var types = nit(nullPsi)
      .flatMap(ref => Option(ref.getQualifier))
      .filter(qual => {
        // filter out cases when caret is _inside_ the qualifier - caret should always be to the right
        val qualEnd = qual.getTextOffset + qual.getTextLength
        qualEnd < parameters.getOriginalPosition.getTextOffset
      })
      .flatMap(qual => exprCtx.findExprType(qual))
    types = types
      .filter(t => {
        if (typesGot == 0) {
          result.addLookupAdvertisement("Resolved first type in " + ((System.nanoTime - startTime) / 1000000000.0) + " s. after " + search.expressionsResolved + " expressions")
        }
        typesGot = typesGot + 1
        true
      })
    val mems = types
      .flatMap(typ => getNamedProps(typ, nullPsi.getProject)
        .filter(prop => {
          if (propsGot == 0) {
            result.addLookupAdvertisement("Resolved first key " + prop.getMemberName + " in " + ((System.nanoTime - startTime) / 1000000000.0) + " s. after " + search.expressionsResolved + " expressions")
          }
          propsGot = propsGot + 1
          true
        })
        .map(p => PropRec(p, isBuiltIn(typ))))

    Console.println("Created property iterator within " + search.expressionsResolved + " expressions (" + typesGot + " types)")
    val builtInSuggestions = getBuiltIns()

    val suggested = new mutable.HashSet[String]()
    mems
      .zipWithIndex
      .map({case (e,i) => makeLookup(e,i)})
      .filter(ourKup => addsTypeInfo(ourKup, builtInSuggestions))
      .foreach(ourKup => {
        result.addElement(ourKup)
        suggested.add(ourKup.getLookupString)
      })

    val elapsed = System.nanoTime - startTime
    result.addLookupAdvertisement("Resolved all in " + (elapsed / 1000000000.0) + " s. after " + search.expressionsResolved + " expressions")
    Console.println("Resolved all in " + (elapsed / 1000000000.0) + " s. after " + search.expressionsResolved + " expressions")

    val keptBuiltIns = builtInSuggestions.asScala
      .flatMap(tuple => {
        val (isGuess, builtInKup) = tuple
        var memName = builtInKup.getLookupString
        if (memName.endsWith("()")) {
          memName = substr(memName, 0, -2)
        }
        if (suggested.contains(memName)) None else {
          Some(builtInKup)
        }
      })
    result.addAllElements(keptBuiltIns.asJava)

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
