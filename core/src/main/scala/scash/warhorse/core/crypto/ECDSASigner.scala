package scash.warhorse.core.crypto

import java.math.BigInteger

import org.bouncycastle.crypto.params.ECPublicKeyParameters
import scash.warhorse.Result
import scodec.bits.ByteVector
import scash.warhorse.core._
import org.bouncycastle.crypto.signers.{ ECDSASigner => BCSigner }

case class ECDSA()

class ECDSASigner(implicit e: ECCurve[Secp256k1]) extends Signer[ECDSA] {
  def sign(msg: ByteVector, privkey: PrivateKey): Result[Signature[ECDSA]] = Predef.???

  def verify(msg: ByteVector, sig: Signature[ECDSA], pubkey: PublicKey): Boolean = {
    val pkeypoint  = e.domain.getCurve.decodePoint(pubkey.bytes.toArray)
    val pkeyparams = new ECPublicKeyParameters(pkeypoint, e.domain)
    val signer     = new BCSigner
    signer.init(false, pkeyparams)
    val (r, s) = sig.getRs
    signer.verifySignature(
      msg.toArray,
      new BigInteger(1, r.bytes.toArray),
      new BigInteger(1, s.bytes.toArray)
    )
  }
}
