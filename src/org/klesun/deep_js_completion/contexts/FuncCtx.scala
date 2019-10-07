package org.klesun.deep_js_completion.contexts

import com.intellij.lang.javascript.psi.types.{JSArrayType, JSArrayTypeImpl, JSTypeSource}
import com.intellij.lang.javascript.psi.{JSCallExpression, JSExpression, JSFunction, JSType}
import com.intellij.psi.PsiElement
import org.klesun.deep_js_completion.contexts.EArgPsiType.EArgPsiType
import org.klesun.deep_js_completion.structures.JSDeepMultiType
import org.klesun.lang.DeepJsLang._

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
  fakeFileSource: Option[PsiElement] = None,
) extends IFuncCtx {
  var hashCodeField: Option[Int] = None

  def getSearch = search

  def subCtxDirect(funcCall: JSCallExpression, findExprType: Function[JSExpression, GenTraversableOnce[JSType]]): FuncCtx = {
    val psiArgs = funcCall.getArguments
    val argGetters = psiArgs.map(psi => cast[JSExpression](psi).itr
          .flatMap(arg => findExprType.apply(arg)).mem()
    ).toList
    FuncCtx(
      search = search,
      parent = Some(this),
      uniqueRef = Some(funcCall),
      argGetters = argGetters,
      argPsiType = EArgPsiType.DIRECT,
      fakeFileSource = fakeFileSource,
    )
  }

  def subCtxEmpty(): FuncCtx = {
    FuncCtx(search = search, parent = Some(this), fakeFileSource = fakeFileSource)
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
      argGetters.lift(order).itr().flatMap(g => g)
    } else {
      None
    }
  }

  /** will return false if function was called with 0 arguments or args unknown */
  override def hasArgs(): Boolean = argGetters.length > 0

  /** will return true if function was called with 0 arguments */
  override def areArgsKnown(): Boolean = !argPsiType.equals(EArgPsiType.NONE)

  override def isInComment(): Boolean = {
    fakeFileSource.nonEmpty
  }

  override def getSpreadArg(): JSArrayType = {
    val elts = argGetters.itr().flatMap(g => g)
    val elt = JSDeepMultiType(elts.mem())
    new JSArrayTypeImpl(elt, JSTypeSource.EMPTY)
  }

  override def getClosurePsi(): Option[JSFunction] = closurePsi
  override def getClosureCtx(): Option[IFuncCtx] = closureCtx

  private def getHashValues(): List[Object] = {
    if (argGetters.isEmpty) {
      closureCtx.toList
    } else {
      closureCtx.toList ++ List(argPsiType) ++ uniqueRef ++ parent
    }
  }

  def printDebug(level: Int = 0): Unit = {
    val step = "  "
    val ind = step * level
    val nextInd = ind + step
    Console.println("FuncCtx(")
    Console.println(nextInd + "id=" + System.identityHashCode(this))
    Console.println(nextInd + "uniqueRef=" + uniqueRef.map(ref => singleLine(ref.getText, 100)))
    Console.print(nextInd + "parent=")
    if (parent.nonEmpty) {
      parent.get.printDebug(level + 1)
    } else {
      Console.println("None")
    }
    Console.print(nextInd + "parent=")
    if (closureCtx.nonEmpty) {
      closureCtx.flatMap(cast[FuncCtx](_)).get.printDebug(level + 1)
    } else {
      Console.println("None")
    }
    Console.println(ind + ")")
  }

  override def hashCode(): Int = {
    if (hashCodeField.isEmpty) {
      val hashValues = getHashValues()
      val hashCode = hashValues.hashCode()
      hashCodeField = Some(hashCode)
    }
    hashCodeField.get
  }

  override def equals(that: Any): Boolean = {
    cast[FuncCtx](that).exists(that => that.getHashValues equals this.getHashValues)
  }
}
