package org.klesun.deep_js_completion.resolvers

import java.util

import com.intellij.lang.javascript.psi.JSRecordType.TypeMember
import com.intellij.lang.javascript.psi._
import com.intellij.lang.javascript.psi.ecmal4.JSAttributeList.ModifierType
import com.intellij.lang.javascript.psi.ecmal4.{JSAttributeList, JSClass}
import com.intellij.lang.javascript.psi.impl.{JSArrayLiteralExpressionImpl, JSLiteralExpressionImpl}
import com.intellij.lang.javascript.psi.types._
import com.intellij.psi.PsiElement
import com.intellij.psi.stubs.StubElement
import org.klesun.deep_js_completion.contexts.IExprCtx
import org.klesun.deep_js_completion.helpers.Mt
import org.klesun.deep_js_completion.structures.{JSDeepClassType, JSDeepMultiType}
import org.klesun.lang.DeepJsLang._

import scala.collection.GenTraversableOnce
import scala.collection.JavaConverters._
import scala.util.Try

case class ThisCls(isStatic: Boolean, clsPsi: JSClass)

object MainRes {

	def getReturns(func: PsiElement): It[JSExpression] = {
		val arrow = cast[JSFunctionExpression](func).itr()
			.flatMap(f => Option(f.getLastChild))
			.flatMap(cast[JSExpression](_))
		val classic = func.getChildren.itr
			.filter(c => !c.isInstanceOf[JSFunction])
			.flatMap(c => getReturns(c) ++ cast[JSReturnStatement](c)
				.flatMap(ret => Option(ret.getExpression)))
		cnc(arrow, classic)
	}

	def getThisCls(expr: JSThisExpression) = {
		var parent = expr.getParent
		var clsOpt: Option[JSClass] = None
		var isStatic = false
		var done = false
		while (parent != null && clsOpt.isEmpty) {
			if (parent.isInstanceOf[JSFunction]) {
				if (cast[JSFunctionExpression](parent).exists(f => f.isShorthandArrowFunction)) {
					// ignore, arrow functions don't have their own this
				} else {
					isStatic = parent.getChildren
						.flatMap(cast[JSAttributeList](_))
						.exists(attrs => attrs.hasModifier(ModifierType.STATIC))
					clsOpt = Option(parent.getParent).flatMap(cast[JSClass](_))
					done = true
				}
			}
			parent = parent.getParent
		}
		clsOpt.map(clsPsi => ThisCls(isStatic, clsPsi))
	}

	def resolveThisExpr(expr: JSThisExpression, ctx: IExprCtx): GenTraversableOnce[JSType] = {
		getThisCls(expr).itr()
			.flatMap(rec => {
				val clst = JSDeepClassType(rec.clsPsi, ctx.subCtxEmpty())
				if (rec.isStatic) Some(clst)
				else clst.getNewInstType(ctx.subCtxEmpty())
			})
	}

	def resolveNewExpr(newex: JSNewExpression, ctx: IExprCtx): GenTraversableOnce[JSType] = {
		if (Option(newex.getMethodExpression).forall(ref => ref.getText equals "Promise")) {
			val types = newex.getArguments.lift(0)
				.flatMap(cbArg => cast[JSFunction](cbArg))
				.flatMap(f => f.getParameters.lift(0))
				.flatMap(cbArg => cast[JSParameter](cbArg))
				.itr.flatMap(par => VarRes.findVarUsages(par))
				.flatMap(usage => Option(usage.getParent)
					.flatMap(cast[JSCallExpression](_))
					.filter(call => usage eq call.getMethodExpression))
				.flatMap(call => call.getArguments.lift(0))
				.flatMap(value => ctx.subCtxEmpty().findExprType(value))
				.map(valuet => Mt.wrapPromise(valuet))
			types
		} else {
			nit(newex.getMethodExpression)
				.flatMap(exp => ctx.findExprType(exp)).itr
				.flatMap(Mt.flattenTypes)
				.flatMap(cast[JSDeepClassType](_))
				.flatMap(clst => clst.getNewInstType(ctx.subCtxDirect(newex)))
		}
	}

	def resolveIn(expr: JSExpression, ctx: IExprCtx): GenTraversableOnce[JSType] = {
		val types: GenTraversableOnce[JSType] = expr match {
			case newex: JSNewExpression => resolveNewExpr(newex, ctx)
			case call: JSCallExpression => FuncCallRes(ctx).resolve(call)
			case vari: JSReferenceExpression => VarRes(ctx).resolve(vari)
			case indx: JSIndexedPropertyAccessExpression =>
				nit(indx.getQualifier)
					.flatMap(qual => ctx.findExprType(qual))
					.flatMap(arrT => {
						val keyTOpt = nit(indx.getIndexExpression)
							.flatMap(lit => ctx.limitResolveDepth(10, lit))
						val result = ctx.mt().getKey(arrT, keyTOpt)
						result
					})
			case func: JSFunctionExpression => FuncRes(ctx).resolve(func)
			case arr: JSArrayLiteralExpressionImpl =>
				val typeTuple = arr.getExpressions
					.flatMap {
						case spr: JSSpreadExpression =>
							val tit = nit(spr.getExpression)
								.flatMap(exp => ctx.findExprType(exp))
								.flatMap(arrt => ctx.mt().getKey(arrt, None))
							val elt = JSDeepMultiType(tit.mem())
							Some(elt)
						case el => frs(ctx.findExprType(el), Some(JSUnknownType.JS_INSTANCE))
					}
					.toList.asJava
				Some(new JSTupleTypeImpl(JSTypeSource.EMPTY, typeTuple, true, -1))
			case obje: JSObjectLiteralExpression =>
				val props: util.List[TypeMember] = obje.getProperties.flatMap(p => {
					val valtit = nit(p.getValue)
						.flatMap(expr => ctx.findExprType(expr))
					Option(p.getName).map(n => Mt.mkProp(n, valtit, Some(p)))
				}).toList.asJava
				val explKeysRec = new JSRecordTypeImpl(JSTypeSource.EMPTY, props)

				val spreadTypes = obje.getPropertiesIncludingSpreads
					.flatMap(cast[JSSpreadExpression](_)).itr()
					.flatMap(spr => nit(spr.getExpression))
					.flatMap(exp => ctx.findExprType(exp))

				cnc(Some(explKeysRec), spreadTypes)
			case bina: JSBinaryExpression =>
				val types = cnc(nit(bina.getLOperand), nit(bina.getROperand))
					.flatMap(op => ctx.findExprType(op))
				types
			case lit: JSLiteralExpressionImpl =>
				if (lit.isBooleanLiteral) {
					Some(new JSBooleanLiteralTypeImpl(lit.getValue.asInstanceOf[Boolean], false, JSTypeSource.EMPTY))
				} else if (lit.isNumericLiteral) {
					Try((lit.getValue + "").toDouble).toOption
						.map(valu => new JSNumberLiteralTypeImpl(valu, false, JSTypeSource.EMPTY, lit.getValue + ""))
				} else {
					None
				}
			case tern: JSConditionalExpression =>
				List(tern.getThen, tern.getElse).itr()
					.flatMap(expr => ctx.findExprType(expr))
			case par: JSParenthesizedExpression =>
				nit(par.getInnerExpression)
					.flatMap(inner => ctx.findExprType(inner))
			case pref: JSPrefixExpression =>
				if ("JS:AWAIT_KEYWORD" equals (pref.getOperationSign + "")) {
					nit(pref.getExpression)
						.flatMap(expr => ctx.findExprType(expr))
						.flatMap(pomiset => Mt.unwrapPromise(pomiset))
				} else {
					None
				}
			case thisExpr: JSThisExpression => resolveThisExpr(thisExpr, ctx)
			case spread: JSSpreadExpression =>
				ctx.findExprType(spread.getExpression).itr()
					.flatMap(t => Mt.flattenTypes(t))
					.flatMap(t => {
						// does not handle {...obj}
						val arrElts: GenTraversableOnce[JSType] = t match {
							case arrt: JSArrayType => Some(arrt.getType)
							case tupt: JSTupleType => tupt.getTypes.asScala
							case _ => None
						}
						arrElts
					})
			case _ => {
				None
			}
		}
		types
	}
}
