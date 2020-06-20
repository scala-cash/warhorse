package scash.warhorse.core.script

import scash.warhorse.core.script.ScriptInterpreter.Done
import scash.warhorse.gen
import scash.warhorse.util._

import zio.test._
import zio.test.Assertion._

object ScriptSpec extends DefaultRunnableSpec {

  val spec = suite("ScriptSpec")(
    testM("syntax")(
      check(gen.pubkey) { pubkey =>
        assert(
          ScriptCompiler.compile(P2PKHScriptPubkey(pubkey)).show
        )(equalTo((OP_DUP >> OP_HASH160).show))
      }
    ),
    testM("interpreter")(
      check(gen.sigAndPubkey) {
        case (sig, p) =>
          val scriptSig    = P2PKhScriptSig(p, sig, 0x00.toByte)
          val scriptPubkey = P2PKHScriptPubkey(p)
          val prog         = ScriptCompiler.compile(scriptSig) ++ ScriptCompiler.compile(scriptPubkey)
          assert(ScriptInterpreter.run(prog))(success(Done))
      }
    )
  )
}
