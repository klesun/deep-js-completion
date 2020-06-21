package org.klesun.deep_js_completion.resolvers.var_res.generic_res

import com.intellij.lang.javascript.psi.JSType
import com.intellij.lang.javascript.psi.ecma6.impl.{TypeScriptArrayTypeImpl, TypeScriptFunctionTypeImpl, TypeScriptTupleTypeImpl}
import com.intellij.lang.javascript.psi.ecma6.{JSTypeDeclaration, TypeScriptObjectType, TypeScriptSingleType, TypeScriptUnionOrIntersectionType}
import com.intellij.lang.javascript.psi.types.{JSStringLiteralTypeImpl, JSTypeSource}
import org.klesun.deep_js_completion.contexts.IExprCtx
import org.klesun.deep_js_completion.helpers.Mt
import org.klesun.lang.DeepJsLang.{MemIt, _}

import scala.collection.GenTraversableOnce

case class ToGetTypeFromExpr(
	ctx: IExprCtx,
	generic: String,

	/** not sure it should be passed here... */
	genericsSrc: collection.mutable.Map[String, () => MemIt[JSType]],
) {
	def applyToSingleType(
		sints: TypeScriptSingleType,
		getArgt: () => GenTraversableOnce[JSType],
	): GenTraversableOnce[JSType] = {
		val fqn = sints.getQualifiedTypeName
		if (generic equals fqn) {
			getArgt()
		} else if (List("Iterable", "ReadonlyArray").contains(fqn)) {
			val getSubType = () => getArgt().itr.flatMap(t => ctx.mt().getKey(t, None))
			sints.getTypeArguments.headOption.itr
				.flatMap(eldec => apply(eldec, getSubType))
		} else if ("ArrayIterator" equals fqn) {
			// lodash; possibly should resolve these custom lib types rather than hardcode them...
			// getArgt() holds the type of the passed function
			val getArgMit = () => getArgt().mem()
			val getArgType = () => {
				// should probably store argument declaration types in deep js function as well... but not sure, no relevant cases
				None
				//getArgMit().itr.flatMap(t => Mt.getArgType(t, ctx, 0))
			}: GenTraversableOnce[JSType]
			val getRetType = () => getArgMit().itr.flatMap(t => Mt.getReturnType(t, ctx))
			cnc(
				sints.getTypeArguments.lift(0).itr()
					.flatMap(argdec => apply(argdec, getArgType)),
				sints.getTypeArguments.lift(1).itr()
					.flatMap(retdec => apply(retdec, getRetType)),
			)
		} else if (
			("PromiseLike" equals fqn) ||
				("Promise" equals fqn) ||
				("Bluebird" equals fqn)
		) {
			val getSubType = () => getArgt().itr.flatMap(t => Mt.unwrapPromise(t))
			sints.getTypeArguments.headOption.itr
				.flatMap(eldec => apply(eldec, getSubType))
		} else if ("any".equals(fqn)) {
			None
		} else {
			//Console.println("Unsupported generic type expr arg class - " + fqn + " - " + sints)
			None
		}
	}

	/**
	 * (arg: SomeCls<T>) => {} -> returns type of T, knowing the complete type of arg
	 *
	 * possibly can cause infinite recursion, should add check some time
	 */
	def apply(
		argTypePsi: JSTypeDeclaration,
		getArgt: () => GenTraversableOnce[JSType],
	): GenTraversableOnce[JSType] = {
		argTypePsi match {
			case union: TypeScriptUnionOrIntersectionType =>
				union.getTypes.itr().flatMap(subTypePsi =>
					apply(subTypePsi, getArgt))
			case obj: TypeScriptObjectType =>
				val getSubType = () => getArgt().itr.flatMap(t => ctx.mt().getKey(t, None))
				obj.getIndexSignatures.itr().flatMap(sig => apply(sig.getType, getSubType))
			case sints: TypeScriptSingleType => applyToSingleType(sints, getArgt)
			case arrts: TypeScriptArrayTypeImpl =>
				val getSubType = () => getArgt().itr.flatMap(t => ctx.mt().getKey(t, None))
				Option(arrts.getType).itr
					.flatMap(eldec => apply(eldec, getSubType))
			case tupts: TypeScriptTupleTypeImpl =>
				val getSubType = (order: Int) => {
					val keyt = new JSStringLiteralTypeImpl(order + "", true, JSTypeSource.EMPTY)
					getArgt().itr().flatMap(t => ctx.mt().getKey(t, Some(keyt)))
				}
				tupts.getElements.zipWithIndex.itr().flatMap({
					case (typePsi, i) => apply(typePsi, () => getSubType(i))
				})
			case func: TypeScriptFunctionTypeImpl =>
				val retts = func.getReturnTypeElement
				val getRetType = () => getArgt().itr().flatMap(t => Mt.getReturnType(t, ctx.subCtxEmpty()))
				// causes 25k expressions on a test case - disabled for now
				//apply(retts, getRetType)
				None
			case _ =>
				//Console.println("Unsupported generic type expr arg kind - " + argTypePsi.getClass + " - " + argTypePsi)
				None
		}
	}
}
