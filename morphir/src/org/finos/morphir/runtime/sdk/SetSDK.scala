package org.finos.morphir.runtime.sdk

import org.finos.morphir.ir.Type
import org.finos.morphir.runtime._
import org.finos.morphir.runtime.internal._
import org.finos.morphir.runtime.RTValue

object SetSDK {

  val foldr = DynamicNativeFunction3("foldr") {
    (context: NativeContext) => (f: RTValue.Function, zero: RTValue, set: RTValue.Set) =>
      // TODO: change foldr to sort before folding.  Currently blocked on comparable support
      // Elm foldr iterates on sort order, Scala iterates on insertion order.  Sort before folding.
      set.value.foldRight(zero) { (a, acc) =>
        context.evaluator.handleApplyResult2(Type.UType.Unit(()), f, a, acc)
      }
  }
}
