package scash.warhorse.core.crypto

import scash.warhorse.gen
import zio.test.DefaultRunnableSpec
import zio.test._
import scash.warhorse.util._
import scash.warhorse.core._

object CryptoSpec extends DefaultRunnableSpec {
  val spec = suite("CryptoSpec")(
    suite("PrivateKeySpec")(
      testM("priv -> bytes -> priv")(check(gen.privKey)(priv => assert(PrivateKey(priv.bytes))(success(priv)))),
      testM("bytes -> priv -> bytes")(
        check(gen.sha256)(priv => assert(priv.decode[PrivateKey].map(_.bytes))(success(priv)))
      ),
      test("fail zero")(assert(PrivateKey(PrivateKey.zero))(failure)),
      test("fail max")(assert(PrivateKey(PrivateKey.max))(failure)),
      testM("fail bytes")(check(gen.byteVectorN(33))(b => assert(PrivateKey(b))(failure)))
    ),
    suite("PublicKeySpec")(
      testM("pub -> bytes -> pub")(check(gen.pubkey)(pub => assert(PublicKey(pub.bytes))(success(pub)))),
      testM("bytes -> pub -> bytes")(
        check(gen.pubkey)(pub => assert(pub.bytes.decode[PublicKey])(success(pub)))
      ),
      testM("fail compressed")(check(gen.byteVectorN(32))(b => assert(PrivateKey(0x01.toByte +: b))(failure))),
      testM("fail uncompressed")(check(gen.byteVectorN(64))(b => assert(PrivateKey(0x01.toByte +: b))(failure)))
    )
  )
}
