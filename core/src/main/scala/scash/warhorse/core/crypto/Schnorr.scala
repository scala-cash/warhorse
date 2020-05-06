package scash.warhorse.core.crypto

import java.math.BigInteger

import org.bouncycastle.crypto.params.ECPrivateKeyParameters
import org.bouncycastle.math.ec.ECPoint

import scash.warhorse.{ Err, Result }
import scash.warhorse.Result.{ Failure, Successful }
import scash.warhorse.core._
import scash.warhorse.core.crypto.hash.Sha256

import scodec.bits.ByteVector

sealed trait Schnorr

object Schnorr {

  class SchnorrSigner(ecc: ECCurve[Secp256k1]) extends Signer[Schnorr] {
    val fieldSize = ecc.domain.getCurve.getField.getCharacteristic

    /** Same additional data used in ABC and bchd for generating the same deterministic schnorr signing */
    val additionalData = "Schnorr+SHA256  ".getBytes("UTF-8").toByteVector

    private def hasSquareY(R: ECPoint) =
      R.getYCoord.toBigInteger
        .modPow(fieldSize.subtract(BigInteger.ONE).divide(BigInt(2L).bigInteger), fieldSize) == BigInteger.ONE

    def sign(unsigned: ByteVector, privkey: PrivateKey): Result[Signature] = {
      val privkeyNum    = privkey.toBigInteger
      val privkeyParams = new ECPrivateKeyParameters(privkeyNum, ecc.domain)
      val N             = ecc.domain.getN
      val G             = ecc.domain.getG

      /** Calculate k*/
      val nonceFunction = nonceRFC6979
      nonceFunction.init(N, privkeyParams.getD, unsigned.toArray, additionalData.toArray)
      val k0 = nonceFunction.nextK.mod(N)
      if (k0.equals(BigInteger.ZERO)) Failure(Err("Fail to generate signature"))

      /** R = k * G. Negate nonce if R.y is not a quadratic residue */
      val R = G.multiply(k0).normalize
      val k = if (hasSquareY(R)) k0 else N.subtract(k0)

      /** e = Hash(R.x || compressed(P) || m) mod n */
      val P        = G.multiply(privkeyNum)
      val pubBytes = P.getEncoded(true).toByteVector
      val rx       = R.getXCoord.toBigInteger.toUnsignedByteVector
      val e        = Sha256.hash(rx ++ pubBytes ++ unsigned).toBigInteger.mod(N)

      /** s = (k + e * priv) mod n */
      val s = e.multiply(privkeyNum).add(k).mod(N).toUnsignedByteVector

      /** Signature = (R.x, s) */
      val sig = rx ++ s
      Successful(Signature(sig))
    }

    def verify(msg: ByteVector, sig: ByteVector, pubkey: PublicKey): Result[Boolean] =
      if (msg.size != 32 || sig.size != 64) Successful(false)
      else {
        val P = ecc.domain.getCurve.decodePoint(pubkey.compress.toArray)
        if (P == null || P.isInfinity) Successful(false)
        else {
          val G      = ecc.domain.getG
          val N      = ecc.domain.getN
          val (r, s) = sig.splitAt(32)
          val rNum   = r.toBigInteger
          val sNum   = s.toBigInteger
          if (rNum.compareTo(fieldSize) >= 0 || sNum.compareTo(N) >= 0) Successful(false)
          else {

            /** e = SHA256(r || compressed(P) || m) mod n */
            val e = Sha256
              .hash(r ++ P.getEncoded(true).toByteVector ++ msg)
              .toBigInteger
              .mod(N)

            /** R = sG - eP */
            val sG = G.multiply(sNum)
            val eP = P.multiply(e)
            val R  = sG.subtract(eP).normalize

            /** Valid if R.x == r */
            if (R.isInfinity) Successful(false)
            else if (!hasSquareY(R)) Successful(false)
            else Successful(R.getXCoord.toBigInteger == rNum)
          }
        }
      }
  }

  implicit val schnorrSigner = new SchnorrSigner(Secp256k1.secp256K1Curve)
}
