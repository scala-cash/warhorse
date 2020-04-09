package scash.warhorse.core.number

import scash.warhorse.core.CNumericUtil._
import scash.warhorse.core._
import scash.warhorse.gen
import scodec.bits.ByteVector
import zio.test.Assertion.{ equalTo, isNone }
import zio.test._

import scala.util.Try

object Int64Spec extends DefaultRunnableSpec {
  val spec = suite("Int64")(
    suite("CNumeric")(
      testM("shiftL")(check(gen.int64, Gen.int(0, 64))(shiftL)),
      testM("shiftR")(check(gen.int64, Gen.int(0, 100))(shiftR)),
      testM("sum")(check(gen.int64, gen.int64)(sum)),
      testM("substract")(check(gen.int64, gen.int64)(substract)),
      testM("multiply")(check(gen.int64, gen.int64)(mult)),
      testM("negation")(check(gen.int64)(i => assert(-i)(equalTo_(Int64(-i.num))))),
      testM("bitwiseInclusive |")(check(gen.int64, gen.int64)(bitwiseInclusive)),
      testM("bitwiseExclusive ^")(check(gen.int64, gen.int64)(bitwiseExclusive)),
      testM("bitwiseAnd &")(check(gen.int64, gen.int64)(bitwiseAnd)),
      test("out of bounds")(outofBounds[Int64]),
      test("test bounds")(testBounds[Int64](BigInt(-9223372036854775808L), BigInt(9223372036854775807L)))
    ),
    suite("Serde")(
      testM("symmetry")(check(gen.int64)(symmetry)),
      testM("symmetryHex")(check(gen.int64)(symmetryHex)),
      test("sym min")(symmetry(Int64.min)),
      test("sym max")(symmetry(Int64.max)),
      test("0")(assert(ByteVector.low(8).decode[Int64])(equalTo_(Int64.zero))),
      test("1")(assert((1.toByte +: ByteVector.low(7)).decode[Int64])(equalTo_(Int64.one))),
      test("-1")(assert(ByteVector.fill(8)(0xFF).decode[Int64])(equalTo_(-Int64.one))),
      test("Int32.max + 1")(
        assert(((ByteVector.low(4) :+ 1.toByte) ++ ByteVector.low(3)).decode[Int64])(
          equalTo_(Int64(4294967296L))
        )
      ),
      test("0xFFFFFFFF  == Int32.max")(
        assert((ByteVector.fill(4)(0xFF) ++ ByteVector.low(4)).decode[Int64])(equalTo_(Int64(4294967295L)))
      ),
      test("0xFF == Int8.max")(assert((0xFF.toByte +: ByteVector.low(7)).decode[Int64])(equalTo_(Int64(255)))),
      test("max to hex")(assert(Int64.max.hex)(equalTo("ffffffffffffff7f"))),
      test("min to hex")(assert(Int64.min.hex)(equalTo("0000000000000080"))),
      test("0xffffffffffffff7f == Int64.max")(
        assert((ByteVector.fill(7)(0xFF) :+ 0x7F.toByte).decode[Int64])(equalTo_(Int64.max))
      ),
      test("0x0000000000000080 == Int64.min")(
        assert((ByteVector.low(7) :+ 0x80.toByte).decode[Int64])(equalTo_(Int64.min))
      ),
      test("too large bytevector 0")(assert(Try(ByteVector.low(9).decodeExactly[Int64]).toOption)(isNone)),
      test("too large bytevector 1")(assert(Try(ByteVector.high(9).decodeExactly[Int64]).toOption)(isNone))
    )
  )
}
