package scash.warhorse.core

import scash.warhorse.Result
import scodec.bits.ByteVector
import scash.warhorse.core.crypto.Secp256k1._

package object crypto {
  def genPrivkey: Result[PrivateKey] = secp256k1KeyGen.genPrivkey

  def genPubkey(publicKey: PrivateKey): Result[PublicKey] = secp256k1KeyGen.genPubkey(publicKey)

  def sign[A: Signer](msg: ByteVector, privkey: PrivateKey): Result[Signature[A]] =
    Signer[A].sign(msg, privkey)

  def verify[A: Signer](msg: ByteVector, signature: Signature[A], pubKey: PublicKey): Boolean =
    Signer[A].verify(msg, signature, pubKey)
}
