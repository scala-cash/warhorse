package scash.warhorse.core.crypto

import scash.warhorse.{ Err, Result }
import scash.warhorse.Result.{ Failure, Successful }
import scash.warhorse.core.number.Uint32
import scash.warhorse.core.typeclass.Serde
import scash.warhorse.core._

case class PrivateKey(u: Uint32)

object PrivateKey {
  def apply(bigInt: BigInt): Result[PrivateKey] =
    for {
      num  <- Uint32.safe(bigInt)
      priv <- apply(num)
    } yield priv

  def apply(n: Uint32): Result[PrivateKey] =
    if (n != Uint32.zero) Successful(new PrivateKey(n))
    else Failure(Err.BoundsError("private key", "it must be less than 32 bytes and not zero", n.toString))

  def apply(bytes: Array[Byte]): Result[PrivateKey] =
    Uint32(bytes.toByteVector).flatMap(apply(_))

  implicit val privateKeySerde: Serde[PrivateKey] =
    Uint32.uint32Serde.narrow(apply(_), _.u)
}
