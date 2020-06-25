package scash.warhorse.core.script

import scash.warhorse.Err
import scash.warhorse.Result.{ Failure, Successful }
import scash.warhorse.core._

import scash.warhorse.core.crypto.hash.Hash160

object CryptoInterpreter {

  def opCheckSig(stack: Stack) = stack.pop2.map(_._3)

  def opEqualVerify(stack: Stack) =
    stack.pop2.flatMap(s =>
      if (s._1 == s._2) Successful(s._3)
      else Failure(Err(s"${s._1} is not equal to ${s._2}"))
    )

  def opHash160(stack: Stack) =
    stack.pop.map(s => s._2.push(PUBKEYHASH(s._1.bytes.tail.hash[Hash160])))

}
