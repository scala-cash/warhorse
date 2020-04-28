package scash.warhorse.core.crypto

import scash.warhorse.core.number.{ Uint32, Uint64 }
import scodec.bits.ByteVector

case class Signature[A](b: ByteVector)

object Signature {

  def getRs[A](sig: Signature[A]): (Uint32, Uint64) = Predef.???
}

trait SignatureSyntax {
  implicit class signatureOps[A](a: Signature[A]) {
    def getRs = Signature.getRs(a)
  }
}
