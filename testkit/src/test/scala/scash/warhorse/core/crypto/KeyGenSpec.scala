package scash.warhorse.core.crypto

import scash.warhorse.util._

import scodec.bits.ByteVector

import zio.test.DefaultRunnableSpec
import zio.test._
import zio.test.Assertion._

object KeyGenSpec extends DefaultRunnableSpec {
  val spec = suite("KeyGenSpec")(
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
    }
  )
}
