package scash.warhorse.core.crypto

import scash.warhorse.Result
import scodec.bits.ByteVector

trait Signer[A] {
  def sign(msg: ByteVector, privkey: PrivateKey): Result[Signature[A]]

  def verify(msg: ByteVector, sig: Signature[A], pubkey: PublicKey): Boolean
}

object Signer {
  def apply[A](implicit signer: Signer[A]): Signer[A] = signer
}
