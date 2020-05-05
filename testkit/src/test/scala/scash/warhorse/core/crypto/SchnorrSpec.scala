package scash.warhorse.core.crypto

import org.scash.secp256k1
import zio.test.DefaultRunnableSpec
import zio.test._
import scodec.bits._
import scash.warhorse.core._
import scash.warhorse.gen
import scash.warhorse.util._
import zio.test.Assertion.isTrue

object SchnorrSpec extends DefaultRunnableSpec {

  val spec = suite("SchnorrSpec")(
    testM("sign") {
      checkM(gen.keyPair, gen.sha256) {
        case ((priv, pub), msg) =>
          val sig = crypto.sign[Schnorr](msg, priv).require
          assertM(secp256k1.verifySchnorr(msg.toArray, sig.bytes.toArray, pub.toArray))(isTrue)
      }
    },
    testM("verify") {
      checkM(gen.keyPair, gen.sha256) {
        case ((priv, pub), msg) =>
          val ver =
            secp256k1
              .signSchnorr(msg.toArray, priv.bytes.toArray)
              .map(s => crypto.verify[Schnorr](msg, ByteVector(s), pub))
          assertM(ver)(success(true))
      }
    },
    test("deter") {
      val data = hex"5255683DA567900BFD3E786ED8836A4E7763C221BF1AC20ECE2A5171B9199E8A"
      val sec  = hex"12B004FFF7F4B69EF8650E767F18F11EDE158148B425660723B9F9A66E61F747"
      val exp =
        hex"2C56731AC2F7A7E7F11518FC7722A166B02438924CA9D8B4D111347B81D0717571846DE67AD3D913A8FDF9D8F3F73161A4C48AE81CB183B214765FEB86E255CE"
      val ans = for {
        priv <- PrivateKey(sec)
        pub  <- priv.genPublicKeyCompressed
        sig  <- crypto.sign[Schnorr](data, priv)
        ver  <- crypto.verify[Schnorr](data, sig.bytes, pub)
      } yield (ver, sig)
      assert(ans.map(_._1))(success(true)) //&&
      assert(ans.map(_._2.bytes))(success(exp))
    },
    testM("detrsign") {
      val msg  = hex"0000000000000000000000000000000000000000000000000000000000000000"
      val priv = hex"0000000000000000000000000000000000000000000000000000000000000001"
      val pk   = PrivateKey(priv).require
      val pub  = pk.genPublicKeyCompressed.require
      val sig  = crypto.sign[Schnorr](msg, pk).require
      val ver  = crypto.verify[Schnorr](msg, sig.bytes, pub).require
      assertM(secp256k1.verifySchnorr(msg.toArray, sig.bytes.toArray, pub.toArray).map(_ && ver))(isTrue)
    }
  )
}
