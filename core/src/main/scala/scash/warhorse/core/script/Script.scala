package scash.warhorse.core.script

import scash.warhorse.core.crypto.{ PublicKey, Signature }
import scash.warhorse.core.crypto.hash.Hash160
import scash.warhorse.core.script.ScriptByteCode._

import scala.language.implicitConversions

trait Script extends Product with Serializable

object Script {
  implicit def toScriptProgram(s: Script): ScriptByteCode = Line(s, End)
}

sealed trait ScriptProgram
case class P2PKHScriptPubkey(pubkeyhash: Hash160)                                  extends ScriptProgram
case class P2PKhScriptSig(pubkey: PublicKey, signature: Signature, hashType: Byte) extends ScriptProgram

object ScriptCompiler {
  def compile(script: ScriptProgram): ScriptByteCode =
    script match {
      case P2PKHScriptPubkey(pubkeyHash) =>
        OP_DUP >> OP_HASH160 >> PUBKEYHASH(pubkeyHash) >> OP_EQUALVERIFY >> OP_CHECKSIG
      case P2PKhScriptSig(pubkey, sig, hashType) =>
        SIG(sig, hashType) >> PUBKEY(pubkey)
    }
}
