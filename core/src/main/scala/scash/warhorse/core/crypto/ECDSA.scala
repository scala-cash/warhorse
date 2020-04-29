package scash.warhorse.core.crypto

import org.bouncycastle.asn1.{ ASN1InputStream, ASN1Integer, DLSequence }
import org.bouncycastle.crypto.params.ECPublicKeyParameters
import org.bouncycastle.crypto.signers.{ ECDSASigner => BCSigner }

import scash.warhorse.Result

import scodec.bits.ByteVector
import scash.warhorse.core._

case class ECDSA()

object ECDSA {
  class ECDSASigner(e: ECCurve[Secp256k1]) extends Signer[ECDSA] {
    def sign(msg: ByteVector, privkey: PrivateKey): Result[Signature] = Predef.???

    def verify(msg: ByteVector, sig: ByteVector, pubkey: PublicKey): Boolean = {
      val pkeypoint  = e.domain.getCurve.decodePoint(pubkey.bytes.toArray)
      val pkeyparams = new ECPublicKeyParameters(pkeypoint, e.domain)
      val signer     = new BCSigner
      signer.init(false, pkeyparams)

      val ans1 = new ASN1InputStream(sig.toArray)
      val seq  = ans1.readObject().asInstanceOf[DLSequence]
      val r    = seq.getObjectAt(0).asInstanceOf[ASN1Integer].getPositiveValue
      val s    = seq.getObjectAt(1).asInstanceOf[ASN1Integer].getPositiveValue
      signer.verifySignature(msg.toArray, r, s)
    }
  }

  implicit val ecdsaSigner: Signer[ECDSA] = new ECDSASigner(Secp256k1.secp256K1Curve)
}
