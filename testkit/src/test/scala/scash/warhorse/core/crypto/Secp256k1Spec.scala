package scash.warhorse.core.crypto

import scash.warhorse.core.crypto
import scash.warhorse.gen
import scash.warhorse.util._
import org.scash.secp256k1
import scodec.bits.ByteVector
import scodec.bits._
import zio.test.DefaultRunnableSpec
import zio.test._
import zio.test.Assertion._

object Secp256k1Spec extends DefaultRunnableSpec {
  val spec = suite("secp256k1")(
    test("testVerifyPos") {
      val data = ByteVector.fromValidHex("CF80CD8AED482D5D1527D7DC72FCEFF84E6326592848447D2DC0B0E87DFC9A90") //sha256hash of "testing"
      val sig = ByteVector.fromValidHex(
        "3044022079BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F817980220294F14E883B3F525B5367756C2A11EF6CF84B730B36C17CB0C56F0AAB2C98589"
      )
      val pubHex = ByteVector.fromValidHex(
        "040A629506E1B65CD9D2E0BA9C75DF9C4FED0DB16DC9625ED14397F0AFC836FAE595DC53F8B0EFE61E703075BD9B143BAC75EC0E19F82A2208CAEB32BE53414C40"
      )
      val pub = PublicKey(pubHex).flatMap(crypto.verify[ECDSA](data, sig, _))
      assert(pub)(success(true))
    },
    test("testVerifyNeg") {
      val data = ByteVector.fromValidHex("CF80CD8AED482D5D1527D7DC72FCEFF84E6326592848447D2DC0B0E87DFC9A91")
      val sig = ByteVector.fromValidHex(
        "3044022079BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F817980220294F14E883B3F525B5367756C2A11EF6CF84B730B36C17CB0C56F0AAB2C98589"
      )
      val pubHex = ByteVector.fromValidHex(
        "040A629506E1B65CD9D2E0BA9C75DF9C4FED0DB16DC9625ED14397F0AFC836FAE595DC53F8B0EFE61E703075BD9B143BAC75EC0E19F82A2208CAEB32BE53414C40"
      )
      val pub = PublicKey(pubHex).flatMap(crypto.verify[ECDSA](data, sig, _))
      assert(pub)(success(false))
    },
    test("testSecKeyVerifyPos") {
      val sec  = ByteVector.fromValidHex("67E56582298859DDAE725F972992A07C6C4FB9F62A8FFF58CE3CA926A1063530")
      val psec = PrivateKey(sec)
      assert(psec)(successResult(psec))
    },
    test("testSecKeyVerifyNeg") {
      val sec  = ByteVector.fromValidHex("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")
      val psec = PrivateKey(sec)
      assert(psec)(failure)
    },
    test("testPubKeyCreatePos") {
      val sec       = ByteVector.fromValidHex("67E56582298859DDAE725F972992A07C6C4FB9F62A8FFF58CE3CA926A1063530")
      val resultArr = PrivateKey(sec).flatMap(_.genPublicKey)
      val expected =
        "04C591A8FF19AC9C4E4E5793673B83123437E975285E7B442F4EE2654DFFCA5E2D2103ED494718C697AC9AEBCFD19612E224DB46661011863ED2FC54E71861E2A6"
      val ans = PublicKey(ByteVector.fromValidHex(expected))
      assert(resultArr)(equalTo(ans))
    },
    test("testPubKeyCreateNeg") {
      val sec       = ByteVector.fromValidHex("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")
      val resultArr = PrivateKey(sec).flatMap(_.genPublicKey)
      assert(resultArr)(failure)
    } /*,
    testM("testSignPos") {
      val data   = ByteVector.fromValidHex("CF80CD8AED482D5D1527D7DC72FCEFF84E6326592848447D2DC0B0E87DFC9A90")
      val secHex = ByteVector.fromValidHex("67E56582298859DDAE725F972992A07C6C4FB9F62A8FFF58CE3CA926A1063530")
      val expected =
        "3045022100F51D069AA46EDB4E2E77773FE364AA2AF6818AF733EA542CFC4D546640A58D8802204F1C442AC9F26F232451A0C3EE99F6875353FC73902C68055C19E31624F687CC"

      val sec  = PrivateKey(secHex)
      val pub  = sec.flatMap(_.genPublicKey).require
      val sign = sec.flatMap(crypto.sign[ECDSA](data, _))

      Predef.println(expected)
      Predef.println(sign.require.b)
      val nsign = secp256k1
        .signECDSA(data.toArray, sec.require.b.toArray)
        .map(b => crypto.verify[ECDSA](data, ByteVector(b), pub).require)

      val nver =
        secp256k1.verifyECDSA(secHex.toArray, sign.require.b.toArray, pub.bytes.toArray)

      assertM(nsign.flatMap(b => nver.map(nb => b && nb)))(isTrue)
    },
    testM("signECDSA") {
      checkM(gen.keyPair, gen.sha256) {
        case ((priv, pub), msg) =>
          val sig   = crypto.sign[ECDSA](msg, priv).require
          val verif = crypto.verify[ECDSA](msg, sig.b, pub)
          Predef.println(s"verif $verif priv ${priv.bytes} sig ${sig.b}, pub ${pub.bytes} msg $msg")
          assertM(secp256k1.verifyECDSA(priv.bytes.tail.toArray, sig.b.toArray, pub.bytes.toArray))(isTrue)
      }
    }*/,
    testM("Deterministic") {
      val msg = "Satoshi Nakamoto"
      checkM(gen.sha256(msg)) {
        data =>
          val priv = hex"0000000000000000000000000000000000000000000000000000000000000001"
          val sig =
            hex"3045022100934b1ea10a4b3c1757e2b0c017d0b6143ce3c9a7e6a4a49860d7a6ab210ee3d802202442ce9d2b916064108014783e923ec36b49743e2ffa1c4496f01a512aafd9e5"
          Predef.println(sig)
          val sec  = PrivateKey(priv)
          val pub  = sec.flatMap(_.genPublicKey).require
          val sign = sec.flatMap(crypto.sign[ECDSA](data, _))
          val nver = secp256k1.verifyECDSA(data.toArray, sig.toArray, pub.bytes.toArray)
          assertM(nver.map(b => sign.require.b == sig && b))(isTrue)
      }
    }
  )
}
