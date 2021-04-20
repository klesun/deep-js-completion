package org.klesun.deep_js_completion.structures

import com.intellij.lang.javascript.psi.JSRecordType.TypeMember
import com.intellij.lang.javascript.psi.ecmal4.JSAttributeList.ModifierType
import com.intellij.lang.javascript.psi.ecmal4.{JSAttributeList, JSClass}
import com.intellij.lang.javascript.psi.types.{JSRecordTypeImpl, JSRecursiveTypeVisitor, JSTypeSource}
import com.intellij.lang.javascript.psi.{JSRecordType, JSRecursiveTypeTransformer, JSResolvedTypeId, JSType}
import com.intellij.psi.PsiElement
import com.intellij.psi.stubs.StubElement
import com.intellij.util
import com.intellij.util.ProcessingContext
import org.klesun.deep_js_completion.contexts.IExprCtx
import org.klesun.deep_js_completion.helpers.Mt
import org.klesun.deep_js_completion.resolvers.{FuncRes, VarRes}
import org.klesun.lang.DeepJsLang._

import scala.collection.GenTraversableOnce
import scala.collection.JavaConverters._

/**
 * could not find anything like this in built-ins surprisingly
 * built-in resolution returns JSLocalNamedType, gives
 * you _any_ type when you call .asRecordType()
 */
case class JSDeepClassType(
	clsPsi: JSClass,
	closureCtx: IExprCtx,
) extends JSType {

	def getTypeText(typeTextFormat: JSType.TypeTextFormat): String = "Deep<" + clsPsi.getName + ">"

	def asRecordType(): JSRecordType = {
		val props = getMethods(closureCtx.subCtxEmpty(), true)
		new JSRecordTypeImpl(JSTypeSource.EMPTY, props.toList.asJava)
	}

	private def getMethods(declCtx: IExprCtx, static: Boolean): GenTraversableOnce[TypeMember] = {
		clsPsi.getFunctions
			.filter(meth => {
				val isStatic = meth.getChildren.flatMap(cast[JSAttributeList](_))
					.exists(lst => lst.hasModifier(ModifierType.STATIC))
				isStatic equals static
			})
			.map(m => Mt.mkProp(m.getName, FuncRes(declCtx).resolve(m), Some(m)))
	}

	def getNewInstType(newCtx: IExprCtx): GenTraversableOnce[JSType] = {
		val declCtx = newCtx.subCtxEmpty()

		val props: GenTraversableOnce[TypeMember] = List() ++
			clsPsi.getFields.map(f => Mt.mkProp(f.getName, Option(f.getInitializer).itr
				.flatMap(expr => declCtx.findExprType(expr)), Some(f))) ++
			getMethods(declCtx, false)

		Some(new JSRecordTypeImpl(JSTypeSource.EMPTY, props.toList.asJava))
	}

	override def getResolvedTypeText: String = ???

	override def resolvedHashCode(): Int = ???

	override def getResolvedTypeId: JSResolvedTypeId = ???

	override def getTypeText: String = ???

	override def accept(jsRecursiveTypeVisitor: JSRecursiveTypeVisitor): Unit = ???

	override def acceptChildren(jsRecursiveTypeVisitor: JSRecursiveTypeVisitor): Unit = ???

	override def resolveClass(): JSClass = ???

	override def getSource: JSTypeSource = ???

	override def isDirectlyAssignableType(jsType: JSType, processingContext: ProcessingContext): Boolean = ???

	override def isEquivalentTo(jsType: JSType, processingContext: ProcessingContext, b: Boolean): Boolean = ???

	override def isEquivalentTo(jsType: JSType, processingContext: ProcessingContext): Boolean = ???

	override def transformTypeHierarchy(function: util.Function[JSType, JSType]): JSType = ???

	override def transformTypeHierarchy(jsRecursiveTypeTransformer: JSRecursiveTypeTransformer): JSType = ???

	override def copyWithStrict(b: Boolean): JSType = ???

	override def substitute(): JSType = ???
}
