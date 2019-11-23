package org.klesun.deep_js_completion.resolvers

import com.intellij.json.psi._
import com.intellij.lang.javascript.psi.JSType
import com.intellij.lang.javascript.psi.types.{JSRecordTypeImpl, JSTypeSource}
import org.klesun.deep_js_completion.helpers.{Mkt, Mt}
import org.klesun.lang.DeepJsLang.{nit, _}

import scala.collection.GenTraversableOnce
import scala.collection.JavaConverters._

case class JsonRes() {
  def resolve(jsonVal: JsonValue): GenTraversableOnce[JSType] = {
    jsonVal match {
      case obj: JsonObject =>
        val mems = obj.getPropertyList.asScala
          .itr().map(jsonProp => {
            val valTit = nit(jsonProp.getValue).flatMap(resolve)
            Mt.mkProp(jsonProp.getName, valTit, Some(jsonProp))
          });
        Some(new JSRecordTypeImpl(JSTypeSource.EMPTY, mems.toList.asJava))
      case arr: JsonArray => Mkt.arr(arr.getValueList.asScala.itr().flatMap(v => resolve(v)))
      case str: JsonStringLiteral => Mkt.str(str.getValue)
      case num: JsonNumberLiteral => Mkt.num(num.getValue)
      case boo: JsonBooleanLiteral => Mkt.bool(boo.getValue)
      case nul: JsonNullLiteral => Mkt.nul()
      case _ => None
    }
  }
}
