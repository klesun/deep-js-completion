package org.klesun.deep_js_completion.completion_providers

import java.util

import com.intellij.codeInsight.completion.{CompletionParameters, CompletionProvider, CompletionResultSet, PrioritizedLookupElement}
import com.intellij.codeInsight.lookup.{LookupElement, LookupElementBuilder}
import com.intellij.codeInsight.navigation.actions.GotoDeclarationHandler
import com.intellij.lang.javascript.psi.JSRecordType.{IndexSignature, PropertySignature, TypeMember}
import com.intellij.lang.javascript.psi.JSType.TypeTextFormat
import com.intellij.lang.javascript.psi.ecma6.impl.{TypeScriptFunctionSignatureImpl, TypeScriptInterfaceImpl}
import com.intellij.lang.javascript.psi.resolve.JSClassResolver
import com.intellij.lang.javascript.psi.types.JSRecordMemberSourceFactory.EmptyMemberSource
import com.intellij.lang.javascript.psi.types.JSRecordTypeImpl.PropertySignatureImpl
import com.intellij.lang.javascript.psi.types._
import com.intellij.lang.javascript.psi.{JSRecordType, JSReferenceExpression, JSType}
import com.intellij.lang.javascript.settings.JSRootConfiguration
import com.intellij.openapi.actionSystem.DataContext
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import com.intellij.psi.search.EverythingGlobalScope
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.util.ProcessingContext
import javax.swing.ImageIcon
import org.klesun.deep_js_completion.completion_providers.PropNamePvdr.{getProps, _}
import org.klesun.deep_js_completion.contexts.SearchCtx
import org.klesun.deep_js_completion.helpers.Mt
import org.klesun.deep_js_completion.structures.DeepIndexSignatureImpl
import org.klesun.lang.Lang._

import scala.collection.{GenIterable, GenTraversableOnce, mutable}
import scala.collection.JavaConverters._
import scala.collection.mutable._

object PropNamePvdr {
//  val imgURL = getClass.getResource("../icons/deep_16.png")
  val imgURL = getClass.getResource("../icons/deep_16_ruby2.png")
  val icon = new ImageIcon(imgURL)
  val DEEP_PRIO = 200

  def getIcon = icon

  private def getMaxDepth(isAutoPopup: Boolean) = {
    if (isAutoPopup) 25 else 40
  }

  private def makeLookup(prop: PropertySignature, i: Int) = {
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

    val lookup = LookupElementBuilder.create(sourceElement, name)
      .bold().withIcon(getIcon)
      .withTypeText(typeStr, true)
    PrioritizedLookupElement.withPriority(lookup, DEEP_PRIO - i)
  }

  def getFlatMems(typ: JSType, project: Project): GenTraversableOnce[TypeMember] = {
    val genMems = Mt.asGeneric(typ, project).itr
      .flatMap(mt => {
        val fqn = mt.getType.getTypeText(TypeTextFormat.CODE)
        val scope = new EverythingGlobalScope(project)
        val tsMems = JSClassResolver.getInstance().findClassesByQName(fqn, scope).asScala
          .itr.flatMap(ifc => ifc.getMembers.asScala)
          .flatMap(cast[TypeMember](_))
        // I suspect just asRecordType() would be enough
        mt.asRecordType().getTypeMembers.asScala ++ tsMems
      })
    var mems = typ match {
      case objT: JSRecordType => objT.getTypeMembers.asScala
      case mt: JSType =>
        // when you specify class with jsdoc for example - JSTypeImpl
        mt.asRecordType().getTypeMembers.asScala
      case _ =>
        /** @debug*/
        //println("Unsupported typ " + typ.getClass + " " + typ)
        List()
    }
    mems = mems ++ genMems
    mems.map {
      case sig: TypeScriptFunctionSignatureImpl =>
        // it implements both PsiElement and TypeMember interfaces at same time
        Mt.mkProp(sig.getMemberName, () => Option(sig.getType), Some(sig))
      case rest => rest
    }
  }

  def getMems(typ: JSType, project: Project): GenTraversableOnce[TypeMember] = {
    Mt.flattenTypes(typ).flatMap(t => getFlatMems(t, project))
  }

  def getProps(typ: JSType, project: Project): It[PropertySignature] = {
    val mems = getMems(typ, project)
    mems.itr().flatMap {
      case prop: PropertySignature => Some(prop)
      case idx: IndexSignature => Mt
        .getAnyLiteralValues(idx.getMemberParameterType)
        .map(name => new PropertySignatureImpl(name, idx.getMemberType, false, new EmptyMemberSource))
      case _ => None
    }
  }
}

class PropNamePvdr extends CompletionProvider[CompletionParameters] with GotoDeclarationHandler {
  override def addCompletions(
    parameters: CompletionParameters,
    context: ProcessingContext,
    result: CompletionResultSet
  ) {

    //val numList = List(1,2,3,4,5)
    //val nums = numList.mem().itr().map(n => Console.println("guzno num " + n))
    //Console.println("zhopa created iterator")
    //nums.foreach(a => Console.println("guzno iterated value"))

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
    val startTime = System.nanoTime
    var typesGot = 0

    val types = nit(nullPsi)
      .flatMap(ref => Option(ref.getQualifier))
      .filter(qual => {
        // filter out cases when caret is _inside_ the qualifier - caret should always be to the right
        val qualEnd = qual.getTextOffset + qual.getTextLength
        qualEnd < parameters.getOriginalPosition.getTextOffset
      })
      .flatMap(qual => search.findExprType(qual))
    val mems = types
      .filter(t => {
        if (typesGot == 0) {
          Console.println("resolved first type in " + ((System.nanoTime - startTime) / 1000000000.0) + " s. after " + search.expressionsResolved + " expressions " + t.getClass + " " + t)
          //throw new RuntimeException("first type was not lazy. why? " + types.getClass)
        } else if (typesGot == 1) {
          Console.println("resolved second type in " + ((System.nanoTime - startTime) / 1000000000.0) + " s. after " + search.expressionsResolved + " expressions " + t.getClass + " " + t)
        } else {
          Console.println("resolved n-th type in " + ((System.nanoTime - startTime) / 1000000000.0) + " s. after " + search.expressionsResolved + " expressions " + t.getClass + " " + t)
        }
        typesGot = typesGot + 1
        true
      })
      .flatMap(typ => getProps(typ, nullPsi.getProject))
      .filter(prop => !prop.getMemberName.startsWith("[Symbol."))

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
  }

  override def getGotoDeclarationTargets(caretPsi: PsiElement, mouseOffset: Int, editor: Editor): Array[PsiElement] = {
    val depth = getMaxDepth(false)
    val search = new SearchCtx(depth, project=Option(editor.getProject))

    nit(caretPsi)
      .flatMap(leaf => Option(leaf.getParent))
      .flatMap(cast[JSReferenceExpression](_))
      .flatMap(ref => nit(ref.getQualifier)
        .flatMap(qual => search.findExprType(qual))
        .flatMap(typ => getMems(typ, caretPsi.getProject))
        .flatMap(cast[DeepIndexSignatureImpl](_))
        .filter(prop => Mt.getAnyLiteralValues(prop.getMemberParameterType)
          .exists(lit => lit equals ref.getReferenceName)))
      .flatMap(p => p.psi)
      .itr().lift(0)
      .toArray
  }

  override def getActionText(dataContext: DataContext): String = null
}
