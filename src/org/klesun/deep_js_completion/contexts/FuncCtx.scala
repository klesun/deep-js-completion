package org.klesun.deep_js_completion.contexts

import com.intellij.lang.javascript.psi.{JSCallExpression, JSExpression, JSFunction, JSType}
import com.intellij.psi.PsiElement
import org.klesun.deep_js_completion.contexts.EArgPsiType.EArgPsiType
import org.klesun.lang.Lang._

import scala.collection.GenTraversableOnce

object EArgPsiType extends Enumeration {
  type EArgPsiType = Value
  val DIRECT, ARR, NONE, INDIRECT = Value
}

case class FuncCtx(
  search: SearchCtx,
  parent: Option[FuncCtx] = None,
  uniqueRef: Option[PsiElement] = None,
  argGetters: List[MemIt[JSType]] = List(),
  argPsiType: EArgPsiType = EArgPsiType.NONE,
  closurePsi: Option[JSFunction] = None,
  closureCtx: Option[IFuncCtx] = None,
) extends IFuncCtx {

  def getSearch = search

  def subCtxDirect(funcCall: JSCallExpression, findExprType: Function[JSExpression, GenTraversableOnce[JSType]]): FuncCtx = {
    val psiArgs = funcCall.getArguments
    val argGetters = psiArgs.map(psi => cast[JSExpression](psi).itr
          .flatMap(arg => findExprType.apply(arg)).mem()
    ).toList
    FuncCtx(search, Some(this), Some(funcCall), argGetters, EArgPsiType.DIRECT)
  }

  def subCtxEmpty(): FuncCtx = {
    FuncCtx(search, Some(this), None, List(), EArgPsiType.NONE)
  }

  def findExprType(expr: JSExpression): GenTraversableOnce[JSType] = {
    val exprCtx = ExprCtx(this, expr, 0)
    search.findExprType(expr, exprCtx)
  }

  def withClosure(closurePsi: JSFunction, closureCtx: IFuncCtx): FuncCtx = {
    this.copy(closurePsi = Some(closurePsi), closureCtx = Some(closureCtx))
  }

  override def getArg(order: Integer): GenTraversableOnce[JSType] = {
    if (order > -1) {
      argGetters.lift(order).itr().flatMap(g => g.itr())
    } else {
      None
    }
  }

  override def getClosurePsi(): Option[JSFunction] = closurePsi
  override def getClosureCtx(): Option[IFuncCtx] = closureCtx
}
