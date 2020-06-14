package scash.warhorse.core

import scash.warhorse.{ Err, Result }
import scash.warhorse.Result.{ Failure, Successful }
import scash.warhorse.core.number.{ Uint5, Uint64 }

import scodec.bits.ByteVector

import Predef._
import scala.util.Try

/**
https://github.com/bitcoincashorg/bitcoincash.org/blob/master/spec/cashaddr.md
 */
case class BCH32(prefix: String, payload: String) {
  override def toString: String = s"$prefix:$payload"
}

object BCH32                                      {
  private val gen = List(
    (0x01, 0x98f2bc8e61L),
    (0x02, 0x79b76d99e2L),
    (0x04, 0xf33e5fb3c4L),
    (0x08, 0xae2eabe2a8L),
    (0x10, 0x1e4f43e470L)
  )

  private val hashSizeMap =
    Vector(160, 192, 224, 256, 320, 384, 448, 512)

  def polyMod(data: Vector[Uint5]): Uint64 = {
    var c = Uint64.one
    data.foreach { d =>
      val c0 = c >> 35
      c = ((c & 0x07ffffffffL) << 5) ^ d
      gen.foreach { case (bit, gen) => if (c0 hasBit bit) c ^= gen }
    }
    c ^ 1
  }

  def verifyCheckSum(prefix: String, payload: String): Boolean =
    Charset
      .fromBase32(payload)
      .map { payloadBytes =>
        val prefixVec  = prefix.map(Uint5.cast).toVector
        val payloadVec = payloadBytes.toArray.map(Uint5(_)).toVector
        val sepVec     = Vector(Uint5.zero)
        polyMod(prefixVec ++ sepVec ++ payloadVec) === Uint64.zero
      }
      .getOrElse(false)

  def calculateCheckSum(prefixVec: Vector[Uint5], payloadVec: Vector[Uint5]): Vector[Uint5] = {
    val sepVec            = Vector(Uint5.zero)
    val chkSumTemplateVec = Vector.fill(8)(Uint5.zero)
    val poly              = polyMod(prefixVec ++ sepVec ++ payloadVec ++ chkSumTemplateVec)

    (0 until 8).toVector
      .map(i => Uint5((poly >> 5 * (7 - i) & 0x1f).num.toByte))
  }

  def fromString(prefix: String, payLoad: String): Result[BCH32] =
    if (verifyCheckSum(prefix, payLoad)) Successful(BCH32(prefix, payLoad))
    else Failure(Err(s"$prefix:$payLoad is not a valid bch32"))

  def genBch32(prefix: String, vtype: Byte, payload: ByteVector): BCH32 = {
    val versionByte   = (vtype | hashSizeMap.indexOf(payload.size * 8)).toByte
    val payloadVec    = bytestoUint5(versionByte +: payload)
    val prefixVec     = prefix.map(Uint5.cast).toVector
    val checkSum      = calculateCheckSum(prefixVec, payloadVec)
    val base32Payload = Charset.toBase32(payloadVec ++ checkSum)

    BCH32(prefix, base32Payload)
  }

  private def bytestoUint5(bytes: ByteVector): Vector[Uint5] = {
    val bits     = bytes.bits
    val fraction = bits.size % 5
    val padding  =
      if (fraction == 0) bits
      else bits.padRight(bits.size + 5 - fraction)

    padding
      .grouped(5)
      .map(_.decode_[Uint5])
      .toVector
  }

  private object Charset {
    // format: off
    private val Chars = Array(
      'q', 'p', 'z', 'r', 'y', '9', 'x', '8',
      'g', 'f', '2', 't', 'v', 'd', 'w', '0',
      's', '3', 'j', 'n', '5', '4', 'k', 'h',
      'c', 'e', '6', 'm', 'u', 'a', '7', 'l'
      )
    // format: on

    def fromBase32(str: String): Result[ByteVector] =
      Result.fromTry(Try(ByteVector(str.map(index))))

    def toBase32(bb: Vector[Uint5]): String = {
      val str = new StringBuffer
      bb.foreach(b => str.append(char(b.num.toInt)))
      str.toString
    }

    def char(i: Int): Char = Chars(i)

    def index(c: Char): Byte = {
      val idx = Chars.indexOf(c)
      if (idx >= 0) idx.toByte
      else throw new IllegalArgumentException
    }
  }

}
