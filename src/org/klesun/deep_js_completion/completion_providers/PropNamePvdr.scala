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

import scala.collection.GenTraversableOnce
import scala.collection.JavaConverters._
import scala.collection.mutable._

object PropNamePvdr {
//  val imgURL = getClass.getResource("../icons/deep_16.png")
  val imgURL = getClass.getResource("../icons/deep_16_ruby2.png")
  val icon = new ImageIcon(imgURL)

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
    PrioritizedLookupElement.withPriority(lookup, 200 - i)
  }

  def getFlatMems(typ: JSType, project: Project): GenTraversableOnce[TypeMember] = {
    val genMems = Mt.asGeneric(typ, project).toList
      .flatMap(mt => {
        val fqn = mt.getType.getTypeText(TypeTextFormat.CODE)
        val scope = new EverythingGlobalScope(project)
        val tsMems = JSClassResolver.getInstance().findClassesByQName(fqn, scope).asScala
          .toList.flatMap(ifc => ifc.getMembers.asScala)
          .flatMap(cast[TypeMember](_))
        // I suspect just asRecordType() would be enough
        mt.asRecordType().getTypeMembers.asScala ++ tsMems
      })
    var mems = typ match {
      case objT: JSRecordType => objT.getTypeMembers.asScala
      case mt: JSTypeBaseImpl =>
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

  private def getProps(typ: JSType, project: Project): List[PropertySignature] = {
    val mems = getMems(typ, project)
    mems.toList.flatMap {
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
    // getPosition() returns element in a _fake_ PSI file with "IntellijIdeaRulezzz " (mind the space in the end) added after the
    // со caret - this may corrupt the PSI tree and give different count of arguments in a function call for example, so no using it!
    //val nullPsi = parameters.getPosition
    val nullPsi = Option(parameters.getOriginalPosition)
      .flatMap(leaf => Option(parameters.getOriginalFile)
        // original PSI points to different elements in `obj.<>prop,`, `obj.prop<>,` and `obj.<>,`, but
        // in all these cases previous PSI is a leaf of the reference expression we want to resolve
        .flatMap(f => Option(PsiTreeUtil.findElementOfClassAtOffset(f, leaf.getTextOffset - 1, classOf[JSReferenceExpression], false))))
      .orNull
    val depth = getMaxDepth(parameters.isAutoPopup)
    val search = new SearchCtx(depth)
    val startTime = System.nanoTime
    val suggestions = Option(nullPsi)
      .flatMap(ref => Option(ref.getQualifier))
      .filter(qual => {
        // filter out cases when caret is _inside_ the qualifier - caret should always be to the right
        val qualEnd = qual.getTextOffset + qual.getTextLength
        qualEnd < parameters.getOriginalPosition.getTextOffset
      })
      .flatMap(qual => search.findExprType(qual))
      .toList.flatMap(typ => getProps(typ, nullPsi.getProject))
      .filter(prop => !prop.getMemberName.startsWith("[Symbol."))
      .zipWithIndex
      .map({case (e,i) => makeLookup(e,i)})

    val elapsed = System.nanoTime - startTime
    result.addLookupAdvertisement("Resolved in " + (elapsed / 1000000000.0) + " seconds")

    // test
    // test
    // test
    // test

    val nameToLookup = ListMap(suggestions.map(t => t.getLookupString -> t) : _*)
    val builtInSuggestions = new util.ArrayList[LookupElement]
    val project = if (nullPsi != null) nullPsi.getProject else null
    val jsConfig = if (project != null) JSRootConfiguration.getInstance(project) else null
    val onlyTyped = if (jsConfig != null) jsConfig.isOnlyTypeBasedCompletion else false

    result.runRemainingContributors(parameters, otherSourceResult => {
      val lookup = otherSourceResult.getLookupElement
      // 99.0 (group 94) - inferred type property completion
      // 5.0 (group 6) - property completion guessed from usage
      val isGuess = cast[PrioritizedLookupElement[LookupElement]](lookup)
        .forall(pri => pri.getPriority < 99.0)

      var memName = lookup.getLookupString
      if (memName.endsWith("()")) {
        memName = substr(memName, 0, -2)
      }
      var keepBuiltIn = true
      if (nameToLookup.contains(memName)) {
        // built-in already suggests this member
        if (onlyTyped && !isGuess) {
          // built-in suggestion is qualitative, keep it, remove ours
          nameToLookup.remove(memName)
        } else {
          // built-in suggestion is mixed with rubbish - remove it, keep ours
          keepBuiltIn = false
        }
      }
      if (keepBuiltIn) {
        builtInSuggestions.add(lookup)
      }
    })

    result.addAllElements(nameToLookup.values.asJava)
    result.addAllElements(builtInSuggestions)
  }

  override def getGotoDeclarationTargets(caretPsi: PsiElement, mouseOffset: Int, editor: Editor): Array[PsiElement] = {
    val depth = getMaxDepth(false)
    val search = new SearchCtx(depth)

    Option(caretPsi)
      .flatMap(leaf => Option(leaf.getParent))
      .flatMap(cast[JSReferenceExpression](_))
      .toList.flatMap(ref => Option(ref.getQualifier)
        .flatMap(qual => search.findExprType(qual))
        .toList.flatMap(typ => getMems(typ, caretPsi.getProject))
        .flatMap(cast[DeepIndexSignatureImpl](_))
        .filter(prop => Mt.getAnyLiteralValues(prop.getMemberParameterType)
          .contains(ref.getReferenceName)))
      .flatMap(p => p.psi)
      .distinct
      .toArray
  }

  override def getActionText(dataContext: DataContext): String = null
}
