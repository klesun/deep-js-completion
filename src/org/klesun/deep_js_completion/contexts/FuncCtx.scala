package org.klesun.deep_js_completion.contexts

import java.util
import java.util.function.Supplier

import com.intellij.lang.javascript.psi.{JSExpression, JSType}
import com.intellij.psi.PsiElement
import org.klesun.deep_js_completion.contexts.EArgPsiType.EArgPsiType

object EArgPsiType extends Enumeration {
  type EArgPsiType = Value
  val DIRECT, ARR, NONE, INDIRECT = Value
}

class FuncCtx(
  val search: SearchCtx,
  val parent: Option[FuncCtx] = None,
  val uniqueRef: Option[PsiElement] = None,
  val argGetters: List[Supplier[JSType]] = List(),
  val variadicOrders: List[Integer] = List(),
  val argPsiType: EArgPsiType = EArgPsiType.NONE,
  val cachedArgs: util.HashMap[Integer, JSType] = new util.HashMap[Integer, JSType](),
) extends ICtx {

  def getSearch = search

  override def findExprType(expr: JSExpression): Option[JSType] = {
    val exprCtx = new ExprCtx(this, expr, 0)
    search.findExprType(expr, exprCtx)
  }
}
