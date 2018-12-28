package org.klesun.deep_js_completion.structures

import com.intellij.lang.javascript.psi.types.JSAnyType
import com.intellij.lang.javascript.psi.types.JSRecordMemberSourceFactory.EmptyMemberSource
import com.intellij.lang.javascript.psi.types.JSRecordTypeImpl.IndexSignatureImpl
import com.intellij.lang.javascript.psi.types.recordImpl.IndexSignatureCommonImpl
import com.intellij.lang.javascript.psi.{JSRecordType, JSType}
import com.intellij.psi.PsiElement

class DeepIndexSignatureImpl(val keyt: JSType, val valt: JSType, val psi: PsiElement) extends IndexSignatureCommonImpl {
  def getIndexSignatureKind: JSRecordType.IndexSignatureKind = IndexSignatureImpl.getIndexerKindFromType(getMemberParameterType)
  def getMemberParameterType: JSType = Option(keyt).getOrElse(JSAnyType.get(null, false))
  def getMemberType: JSType = Option(valt).getOrElse(JSAnyType.get(null, false))
  def getMemberSource: JSRecordType.MemberSource = new EmptyMemberSource
}
