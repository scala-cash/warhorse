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
    testM("scriptSerdeRoundtrip")(
      check(gen.p2pkhScript) {
        case (scriptSig, scriptPubkey) =>
          assert(scriptSig.bytes.decode[P2PKHScriptSig])(success(scriptSig)) &&
            assert(scriptPubkey.bytes.decode[P2PKHScriptPubkey])(success(scriptPubkey))
      }
    )
  )
}
