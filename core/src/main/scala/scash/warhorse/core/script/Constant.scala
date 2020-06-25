package scash.warhorse.core.script

import scash.warhorse.Err
import scash.warhorse.Result.{ Failure, Successful }
import scash.warhorse.core.crypto.hash.Hash160
import scash.warhorse.core.crypto.{ PublicKey, Signature }
import scash.warhorse.core.number.Uint8
import scash.warhorse.core._
import scash.warhorse.core.typeclass.Serde

import scodec.bits.ByteVector
import scodec.codecs._

sealed trait Constant                           extends Script
case class PUBKEYHASH(hash: Hash160)            extends Constant
case class PUBKEY(pubkey: PublicKey)            extends Constant
case class SIG(sig: Signature, hashtype: Byte)  extends Constant
case class BYTES(byteVector: ByteVector)        extends Constant
case object OP_FALSE                            extends Constant
case object OP_TRUE                             extends Constant
case class OP_PUSHDATA1(byteVector: ByteVector) extends Constant

object Constant {

  val opFalseSerde    = Serde.constant(Op.False, OP_FALSE)
  val opTrueSerde     = Serde.constant(Op.True, OP_TRUE)
  val opPushDataSerde = Serde(constant(Op.PushData1) ~> variableSizeBytes(uint8, bytes.as[OP_PUSHDATA1]))

  //TODO: need to implement ECDSA and Schnorr decoding
  val sigSerde = Serde(
    bytes(data.SigPub).xmap[SIG](b => SIG(Signature(b.init), b.last), s => s.sig.bytes :+ s.hashtype)
  )

  val pubkeyHashSerde = Serde(Hash160.hash160Serde.codec.as[PUBKEYHASH])
  val pubkeySerde     = Serde(PublicKey.publicKeySerde.codec.as[PUBKEY])

  private object data {
    val SigPub     = 65
    val Pubkey     = 33
    val PubkeyHash = 20
  }

  private def withSize(b: ByteVector) = Uint8(b.size).bytes ++ b

  implicit val constantSerde =
    Serde[Constant](
      (a: Constant) =>
        a match {
          case OP_FALSE          => opFalseSerde.encode(OP_FALSE)
          case h: PUBKEYHASH     => pubkeyHashSerde.encode(h).map(withSize)
          case p: PUBKEY         => pubkeySerde.encode(p).map(withSize)
          case s: SIG            => sigSerde.encode(s).map(withSize)
          case d: OP_PUSHDATA1   => opPushDataSerde.encode(d)
          case OP_TRUE           => opTrueSerde.encode(OP_TRUE)
          case BYTES(byteVector) => Successful(withSize(byteVector))
        },
      (vec: ByteVector) =>
        ByteVector(vec.head)
          .decode[Uint8]
          .flatMap(h =>
            h.num match {
              case Op.False        => opFalseSerde.decode(vec.tail)
              case data.PubkeyHash => pubkeyHashSerde.decode(vec.tail)
              case data.Pubkey     => pubkeySerde.decode(vec.tail)
              case data.SigPub =>
                pubkeySerde.decode(vec.tail) orElse
                  sigSerde.decode(vec.tail) orElse
                  Serde(bytes(data.SigPub).as[BYTES]).decode(vec.tail)
              case i if i >= 1 && i <= Op.Data => Serde(bytes(i).as[BYTES]).decode(vec.tail)
              case Op.PushData1                => opPushDataSerde.decode(vec.tail)
              case Op.True                     => opTrueSerde.decode(vec.tail)
              case _                           => Failure(Err(s"Invalid Constant size or opcode: ${vec.head}"))
            }
          )
    )

}
