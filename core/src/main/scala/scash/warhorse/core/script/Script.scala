package scash.warhorse.core.script

import scash.warhorse.{ Err, Result }
import scash.warhorse.Result.{ Failure, Successful }
import scash.warhorse.core.crypto.{ PublicKey, Signature }
import scash.warhorse.core.crypto.hash.Hash160
import scash.warhorse.core.typeclass.{ ScriptByteCode, Serde }
import scodec.bits.ByteVector

trait Script extends Product with Serializable { self =>
  def >>(s: Script): ScriptChunk = ScriptChunk(self, s)

  def as[A <: Script]: A = self.asInstanceOf[A]
}

object Script {
  implicit val scriptSerde = Serde[Script](
    (s: Script) =>
      s match {
        case c: Constant   => Constant.constantSerde.encode(c)
        case op: Operation => Operation.operationSerde.encode(op)
        case _             => Failure(Err(s"Not recognized script: $s"))
      },
    (vec: ByteVector) =>
      //Predef.println(vec)
      Constant.constantSerde.decode(vec) orElse Operation.operationSerde.decode(vec)
  )
}

case class P2PKHScriptPubkey(pubkeyhash: Hash160)
case class P2PKHScriptSig(pubkey: PublicKey, signature: Signature, hashType: Byte)

object P2PKHScriptSig {
  implicit val p2PKHScriptSig = new ScriptByteCode[P2PKHScriptSig] {
    def fromASM(scriptChunk: ScriptChunk): Result[P2PKHScriptSig] =
      for {
        sigR <- scriptChunk.head
        pub  <- scriptChunk.at(1)
        ans <- (sigR, pub) match {
          case (SIG(sig, hash), PUBKEY(pubkey)) => Successful(P2PKHScriptSig(pubkey, sig, hash))
          case _                                => Failure(Err("This is not a valid P2P2ScriptSig"))
        }
      } yield ans

    def toASM(p: P2PKHScriptSig): ScriptChunk = SIG(p.signature, p.hashType) >> PUBKEY(p.pubkey)
  }
}

object P2PKHScriptPubkey {
  implicit val p2pkhScriptPubkey = new ScriptByteCode[P2PKHScriptPubkey] {
    def fromASM(scriptChunk: ScriptChunk): Result[P2PKHScriptPubkey] =
      scriptChunk.at(2).flatMap { s =>
        if ((OP_DUP >> OP_HASH160 >> s >> OP_EQUALVERIFY >> OP_CHECKSIG) == scriptChunk)
          Successful(P2PKHScriptPubkey(s.as[PUBKEYHASH].hash))
        else Failure(Err(s"Script $scriptChunk is not a p2pkhScriptPubkey"))
      }

    def toASM(p: P2PKHScriptPubkey): ScriptChunk =
      OP_DUP >> OP_HASH160 >> PUBKEYHASH(p.pubkeyhash) >> OP_EQUALVERIFY >> OP_CHECKSIG

  }
}
