package scash.warhorse.core.crypto

import scash.warhorse.{ Err, Result }
import scash.warhorse.Result.{ Failure, Successful }
import scash.warhorse.core.typeclass.Serde
import scodec.DecodeResult
import scodec.bits.ByteVector

sealed trait PublicKey

case class PubKey(b: ByteVector)             extends PublicKey
case class PubKeyUnCompressed(b: ByteVector) extends PublicKey

object PublicKey {

  def apply(b: ByteVector): Result[PublicKey] =
    if (isCompressed(b)) Successful(PubKey(b))
    else if (isUnCompressed(b)) Successful(PubKeyUnCompressed(b))
    else Failure(Err.BoundsError("publickey", "is not compressed nor uncompressed", b.toHex))

  private def isCompressed(b: ByteVector): Boolean =
    b.size == 33 && (b(0) == 0x02 || b(0) == 0x03)

  private def isUnCompressed(b: ByteVector): Boolean =
    b.size == 65 && (b(0) == 0x04)

  implicit val publicKeySerde: Serde[PublicKey] =
    Serde(
      _ match {
        case PubKey(b)             => Successful(b)
        case PubKeyUnCompressed(b) => Successful(b)
      },
      b =>
        if (b.isEmpty) Failure(Err.ParseError("Publickey", s"empty pubkeys not allowed"))
        else if (List(0x02, 0x03).contains(b(0))) Successful(DecodeResult(PubKey(b.take(33)), b.drop(33).toBitVector))
        else if (b(0) == 0x04) Successful(DecodeResult(PubKeyUnCompressed(b.take(65)), b.drop(65).toBitVector))
        else Failure(Err.ParseError("Publickey", s"$b is not compressed or uncompressed"))
    )
}
