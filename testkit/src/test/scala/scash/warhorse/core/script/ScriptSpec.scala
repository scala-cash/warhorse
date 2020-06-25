package scash.warhorse.core.script

import scash.warhorse.gen
import scash.warhorse.core._
import scash.warhorse.core.crypto.hash.Hash160
import scash.warhorse.core.script.ScriptInterpreter.Done
import scash.warhorse.util._

import zio.test._
import zio.test.Assertion._

import scodec.bits._

object ScriptSpec extends DefaultRunnableSpec {

  val spec = suite("ScriptSpec")(
    testM("interpreter")(
      check(gen.sigAndPubkey) {
        case (sig, p) =>
          val scriptSig    = P2PKHScriptSig(p, sig, 0x00.toByte)
          val scriptPubkey = P2PKHScriptPubkey(p.hash[Hash160])
          val prog         = scriptSig.toASM ++ scriptPubkey.toASM
          assert(ScriptInterpreter.run(prog))(equalTo(Done))
      }
    ),
    testM("scriptASMRoundtrip")(
      check(gen.p2pkhScript) {
        case (scriptSig, scriptPubkey) =>
          assert(scriptSig.toASM.to[P2PKHScriptSig])(success(scriptSig)) &&
            assert(scriptPubkey.toASM.to[P2PKHScriptPubkey])(success(scriptPubkey))
      }
    ),
    test("scriptPubkeySerde")(
      assert(hex"76a914bee4182d9fbc8931a728410a0cd3e0f340f2995a88ac".decode[P2PKHScriptPubkey])(
        success(P2PKHScriptPubkey(hex"bee4182d9fbc8931a728410a0cd3e0f340f2995a".decode_[Hash160]))
      )
    )
  )
}
