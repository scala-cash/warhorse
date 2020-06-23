package scash.warhorse.core.script

import scash.warhorse.gen
import scash.warhorse.core._
import scash.warhorse.core.crypto.hash.Hash160
import scash.warhorse.core.script.ScriptInterpreter.Done
import scash.warhorse.util._
import zio.test._

object ScriptSpec extends DefaultRunnableSpec {

  val spec = suite("ScriptSpec")(
    testM("interpreter")(
      check(gen.sigAndPubkey) {
        case (sig, p) =>
          val scriptSig    = P2PKhScriptSig(p, sig, 0x00.toByte)
          val scriptPubkey = P2PKHScriptPubkey(p.hash[Hash160])
          val prog         = ScriptCompiler.compile(scriptSig) ++ ScriptCompiler.compile(scriptPubkey)
          assert(ScriptInterpreter.run(prog))(success(Done))
      }
    )
  )
}
