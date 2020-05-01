package scash.warhorse.core.crypto

import scash.warhorse.Result
import scodec.bits.ByteVector

class Schnorr extends Signer[Schnorr] {
  def sign(msg: ByteVector, privkey: PrivateKey): Result[Signature] = Predef.???

  def verify(msg: ByteVector, sig: ByteVector, pubkey: PublicKey): Result[Boolean] = Predef.???
}
