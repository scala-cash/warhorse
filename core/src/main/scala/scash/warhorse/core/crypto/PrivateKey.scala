package scash.warhorse.core.crypto

import scash.warhorse.{ Err, Result }
import scash.warhorse.Result.{ Failure, Successful }
import scash.warhorse.core.typeclass.Serde
import scash.warhorse.core._
import scodec.DecodeResult
import scodec.bits.ByteVector

protected[crypto] case class PrivateKey(b: BigInt)

object PrivateKey {

  def apply(b: ByteVector): Result[PrivateKey] = apply(b.toBigInt)

  def apply(bigInt: BigInt): Result[PrivateKey] =
    if (bigInt > zero && bigInt < max) Successful(new PrivateKey(bigInt))
    else Failure(Err.BoundsError("Privatekey", s"needs > $zero && < $max", bigInt.toHex))

  implicit val sha256Serde: Serde[PrivateKey] = Serde[PrivateKey](
    (a: PrivateKey) => Successful(a.b.toByteVector),
    (b: ByteVector) => apply(b.take(32).toBigInt).map(DecodeResult(_, b.drop(32).bits))
  )

  val max  = BigInt("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141", 16)
  val zero = BigInt(0)

  implicit class PrivateKeyOps(p: PrivateKey) {
    def genPublicKey: Result[PublicKey] = crypto.genPubkey(p)
  }
}
