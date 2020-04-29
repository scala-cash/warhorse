package scash.warhorse.core.crypto

import java.math.BigInteger
import java.security.{ SecureRandom, Security }

import org.bouncycastle.crypto.params.ECDomainParameters
import org.bouncycastle.jce.ECNamedCurveTable
import org.bouncycastle.jce.provider.BouncyCastleProvider
import scash.warhorse.Result
import scash.warhorse.core._

case class Secp256k1()

object Secp256k1 {

  val secp256K1Curve: ECCurve[Secp256k1] = {
    Security.insertProviderAt(new BouncyCastleProvider, 1)
    val spec = ECNamedCurveTable.getParameterSpec("secp256k1")
    new ECCurve[Secp256k1] {
      val domain = new ECDomainParameters(spec.getCurve, spec.getG, spec.getN)
    }
  }

  implicit val secp256k1KeyGen: KeyGen[Secp256k1] = new KeyGen[Secp256k1] {
    def genPrivkey: Result[PrivateKey] =
      PrivateKey.apply(BigInt(256, new SecureRandom()))

    def genPubkey(privateKey: PrivateKey): Result[PublicKey] = {
      val pointQ = secp256K1Curve.domain.getG.multiply(new BigInteger(1, privateKey.bytes.toArray))
      PublicKey(pointQ.getEncoded(false).toByteVector)
    }

  }
}
