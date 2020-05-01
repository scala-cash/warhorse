package scash.warhorse.core.crypto

import io.circe.Decoder

import org.scash.secp256k1

import scash.warhorse.core.crypto
import scash.warhorse.gen
import scash.warhorse.util._

import scodec.bits.ByteVector

import zio.ZIO
import zio.test.Assertion._
import zio.test._

object ECDSASpec extends DefaultRunnableSpec {
  val spec = suite("ECDSASpec")(
    testM("sign") {
      checkM(gen.keyPair, gen.sha256) {
        case ((priv, pub), msg) =>
          val sig = crypto.sign[ECDSA](msg, priv).require
          assertM(secp256k1.verifyECDSA(msg.toArray, sig.b.toArray, pub.bytes.toArray))(isTrue)
      }
    },
    testM("verify") {
      checkM(gen.keyPair, gen.sha256) {
        case ((priv, pub), msg) =>
          val ver =
            secp256k1
              .signECDSA(msg.toArray, priv.bytes.toArray)
              .map(s => crypto.verify[ECDSA](msg, ByteVector(s), pub))
          assertM(ver)(success(true))
      }
    },
    testM("deterministic")(
      // dataset from https://bitcointalk.org/index.php?topic=285142.msg3299061#msg3299061
      parseJsonfromFile[List[TestVectorECDSA]]("ecdsa.json")
        .flatMap(r =>
          ZIO.foreach(r.require)(data =>
            check(gen.sha256(data.msgStr)) { msg =>
              val ans = for {
                sec <- PrivateKey(data.privHex)
                pub <- sec.genPublicKey
                sig <- crypto.sign[ECDSA](msg, sec)
                ver <- crypto.verify[ECDSA](msg, sig.b, pub)
              } yield (sig, ver)
              assert(ans)(success((Signature(data.exp), true)))
            }
          )
        )
        .map(BoolAlgebra.collectAll(_).get)
    )
  )

  case class TestVectorECDSA(privHex: ByteVector, msgStr: String, exp: ByteVector)

  implicit val ecdsaDecoder: Decoder[List[TestVectorECDSA]] = csvDecoder.map(dataset =>
    dataset.map { data =>
      TestVectorECDSA(
        ByteVector.fromValidHex(data(0)).padLeft(32),
        data(1),
        ECDSA
          .compact2Der(ByteVector.fromValidHex(data(2)))
          .getOrElse(ByteVector.fromValidHex(data(2)))
      )
    }
  )
}
