package org.klesun.deep_js_completion.resolvers

import com.intellij.json.psi.JsonFile
import com.intellij.lang.javascript.psi._
import com.intellij.lang.javascript.psi.types._
import org.klesun.deep_js_completion.contexts.IExprCtx
import org.klesun.deep_js_completion.entry.PathStrGoToDecl
import org.klesun.deep_js_completion.helpers.{Mkt, Mt}
import org.klesun.deep_js_completion.structures.{EInstType, JSDeepModuleTypeImpl, JSDeepMultiType}
import org.klesun.lang.DeepJsLang._

import scala.collection.GenTraversableOnce
import scala.collection.JavaConverters._
import scala.collection.immutable.List

/**
 * resolves type of a function call expression like:
 * someVar(arg1, arg2)
 */
case class FuncCallRes(ctx: IExprCtx) {

	def resolveBuiltInMethCall(obj: JSExpression, methName: String, args: List[JSExpression]): GenTraversableOnce[JSType] = {
		// Unsupported: reduce, concat, shift, pop
		if (List("filter", "sort", "slice", "splice").contains(methName)) {
			ctx.findExprType(obj)
		} else if (List("concat").contains(methName)) {
			val types = (List(obj) ++ args).flatMap(expr => ctx.findExprType(expr))
			types
		} else if (List("map").contains(methName)) {
			args.lift(0).itr.flatMap(arg => ctx.findExprType(arg))
				.flatMap(funcT => Mt.getReturnType(funcT, ctx.subCtxEmpty()))
				.map(elT => new JSArrayTypeImpl(elT, JSTypeSource.EMPTY))
		} else if (List("flatMap").contains(methName)) {
			args.lift(0).itr.flatMap(arg => ctx.findExprType(arg))
				.flatMap(funcT => Mt.getReturnType(funcT, ctx.subCtxEmpty()))
		} else if (List("reduce").contains(methName)) {
			args.lift(0).itr.flatMap(arg => ctx.findExprType(arg))
				.flatMap(funcT => Mt.getReturnType(funcT, ctx.subCtxEmpty()))
		} else if ((obj.getText equals "Object") && (methName equals "assign")) {
			// IDEA actually has the built-in function generic return type mapping, but I'm
			// not able to get the info (getReturnType returns AnyType instead of A & B)
			args.itr().flatMap(arg => ctx.findExprType(arg))
		} else if ((obj.getText equals "Object") && (methName equals "entries")) {
			// d.ts has the generic for value, but not for key sadly
			args.lift(0).itr().flatMap(arg => ctx.findExprType(arg))
				.map(objt => {
					val elts: GenTraversableOnce[JSType] = Mt
						.getProps(objt, obj.getProject).map(prop => {
						val tts = List(prop.getMemberParameterType, prop.getMemberType)
						new JSTupleTypeImpl(JSTypeSource.EMPTY, tts.asJava, true, -1)
					})
					val elt = JSDeepMultiType(elts.mem())
					new JSArrayTypeImpl(elt, JSTypeSource.EMPTY)
				})
		} else if ((obj.getText equals "Promise") && (methName equals "reject")) {
			// could keep track of type of passed exception at some point, but for now just
			// not suggesting exception properties in successful promise shall be great
			Mkt.inst("Promise", List())
		} else if (methName equals "then") {
			args.lift(0).itr.flatMap(arg => ctx.findExprType(arg))
				.flatMap(funcT => Mt.getReturnType(funcT, ctx.subCtxEmpty()))
				.flatMap(value => Mt.unwrapPromise(value))
				.map(value => Mt.wrapPromise(value))
		} else if ((methName equals "catch") || (methName equals "finally")) {
			ctx.findExprType(obj) // Promise .catch(), could actually add the type from callback, but nah for now
		} else {
			None
		}
	}

	def resolveBuiltInFuncCall(funcName: String, args: List[JSExpression]): GenTraversableOnce[JSType] = {
		if (List("require").contains(funcName)) {
			val types = args.lift(0).itr.flatMap(arg => cnc(
				cnc(
					PathStrGoToDecl.getReferencedFileAnyDir(arg),
					PathStrGoToDecl.getModuleFile(arg)
				).flatMap(file => {
					cnc(
						cast[JSFile](file).itr()
							.flatMap(jsFile => ModuleRes(ctx.subCtxEmpty())
								.resolveCommonJsFormatDef(jsFile)),
						cast[JsonFile](file)
							.flatMap(jsonFile => Option(jsonFile.getTopLevelValue)).itr()
							.flatMap(jsonVal => JsonRes().resolve(jsonVal))
					)
				}),
				cast[JSLiteralExpression](arg)
					.map(lit => JSDeepModuleTypeImpl(lit.getValue + "", EInstType.Required))
			))
			types
		} else {
			None
		}
	}

	def resolve(funcCall: JSCallExpression): GenTraversableOnce[JSType] = {
		nit(funcCall.getMethodExpression)
			.flatMap(expr => {
				val definedRts = ctx.findExprType(expr)
					.itr.flatMap(funcT => Mt.getReturnType(funcT, ctx.subCtxDirect(funcCall)))
				val builtInRts = cast[JSReferenceExpression](expr).itr
					.flatMap(ref => nit(ref.getReferenceName)
						.flatMap(name => Option(ref.getQualifier) match {
							case Some(qual) => resolveBuiltInMethCall(qual, name, funcCall.getArguments.toList)
							case None => resolveBuiltInFuncCall(name, funcCall.getArguments.toList)
						}))

				definedRts ++ builtInRts
			})
	}
}
