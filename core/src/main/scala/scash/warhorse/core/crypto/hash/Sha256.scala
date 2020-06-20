package scash.warhorse.core.crypto.hash

import org.bouncycastle.crypto.Digest
import org.bouncycastle.crypto.digests.SHA256Digest
import scash.warhorse.Err
import scash.warhorse.Result.{ Failure, Successful }
import scash.warhorse.core.typeclass.{ Hasher, Serde }
import scodec.DecodeResult
import scodec.bits.ByteVector

case class Sha256(private[crypto] val b: ByteVector)

case class Sha256B(private[crypto] val b: ByteVector)

object Sha256 {
  private val bsize = 32L

  implicit val sha256Serde: Serde[Sha256] = Serde[Sha256](
    (a: Sha256) => Successful(a.b),
    (b: ByteVector) =>
      if (b.size >= bsize) Successful(DecodeResult(Sha256(b.take(bsize)), b.drop(bsize).bits))
      else Failure(Err.BoundsError("sha256", s"needs $bsize bytes", s"${b.size} bytes"))
  )
  implicit val sha256SerdeB: Serde[Sha256B] =
    sha256Serde.xmap(b => Sha256B(b.b.reverse), a => Sha256(a.b.reverse))

  implicit val sha256hash = new Hasher[Sha256] {
    def cons(b: ByteVector): Sha256 = Sha256(b)
    def hasher: Digest              = new SHA256Digest
  }
}
