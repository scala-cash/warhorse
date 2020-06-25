package scash.warhorse.gen.script

import scash.warhorse.core.crypto.hash.Hash160
import scash.warhorse.core.script.{ P2PKHScriptPubkey, P2PKHScriptSig }
import scash.warhorse.gen

trait ScriptGen {
  def p2pkhScript =
    gen.sigAndPubkeyCompressed.map {
      case (sig, pubkey) =>
        (P2PKHScriptSig(pubkey, sig, 0x00.toByte), P2PKHScriptPubkey(pubkey.hash[Hash160]))
    }
}
