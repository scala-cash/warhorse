package scash.warhorse.core.crypto

import scash.warhorse.Result
import scodec.bits.ByteVector

class Schnorr extends Signer[Schnorr] {
  def sign(msg: ByteVector, privkey: PrivateKey): Result[Signature[Schnorr]] = Predef.???

  def verify(msg: ByteVector, sig: Signature[Schnorr], pubkey: PublicKey): Boolean = Predef.???
}
