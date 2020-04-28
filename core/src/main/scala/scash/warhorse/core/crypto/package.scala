package scash.warhorse.core

import scash.warhorse.Result
import scodec.bits.ByteVector
import scash.warhorse.core.crypto.Secp256k1._

package object crypto {
  def genPrivkey: PrivateKey = secp256k1KeyGen.genPrivkey

  def genPubkey(publicKey: PrivateKey): PublicKey = secp256k1KeyGen.genPubkey(publicKey)

  def isValid(pubkey: PublicKey): Boolean = secp256k1KeyGen.isValidPubkey(pubkey)

  def isValid(privkey: PrivateKey): Boolean = secp256k1KeyGen.isValidPrivatekey(privkey)

  def sign[A: Signer](msg: ByteVector, priv: PrivateKey): Result[Signature[A]] =
    Signer[A].sign(msg, priv)

  def verify[A: Signer](msg: ByteVector, signature: Signature[A], pubKey: PublicKey): Boolean =
    Signer[A].verify(msg, signature, pubKey)
}
