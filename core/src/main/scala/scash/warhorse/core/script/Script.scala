package scash.warhorse.core.script

import scash.warhorse.{ Err, Result }
import scash.warhorse.Result.Failure
import scash.warhorse.core.crypto.{ PublicKey, Signature }
import scash.warhorse.core.crypto.hash.Hash160
import scash.warhorse.core._
import scash.warhorse.core.typeclass.{ Serde, Show }
import scodec.bits.ByteVector
import scodec.codecs._

import scala.language.implicitConversions
import Predef.augmentString

sealed trait Script extends Product with Serializable

case class OP_PUSHDATA1(c: CONSTANT) extends Script

object Script {
  implicit def toScriptProgram(s: Script): ScriptByteCode = Append(s, End)
}

sealed trait OP_CODE       extends Script
case object OP_DUP         extends OP_CODE
case object OP_HASH160     extends OP_CODE
case object OP_EQUALVERIFY extends OP_CODE
case object OP_CHECKSIG    extends OP_CODE

object OP_CODE {
  implicit val opCode: Serde[OP_CODE] =
    Serde(uint8.xmapc {
      case 118 => OP_DUP
      case 136 => OP_EQUALVERIFY
      case 169 => OP_HASH160
      case 172 => OP_CHECKSIG
    } {
      case OP_DUP         => 118
      case OP_EQUALVERIFY => 136
      case OP_HASH160     => 169
      case OP_CHECKSIG    => 172
    })
}

sealed trait CONSTANT                          extends Script
case class PUBKEYHASH(hash: Hash160)           extends CONSTANT
case class PUBKEY(pubkey: PublicKey)           extends CONSTANT
case class SIG(sig: Signature, hashtype: Byte) extends CONSTANT

object CONSTANT {

  val pubkeySerde     = PublicKey.publicKeySerde.xmap[PUBKEY](PUBKEY(_), _.pubkey)
  val pubkeyHashSerde = Hash160.hash160Serde.xmap[PUBKEYHASH](PUBKEYHASH(_), _.hash)
  val sigSerde        = Serde(bytes(65).xmap[SIG](b => SIG(Signature(b.init), b.last), s => s.sig.bytes :+ s.hashtype))
  val sigOrPubKeySerde =
    Serde[CONSTANT](
      (a: CONSTANT) =>
        a match {
          case p: PUBKEY => pubkeySerde.encode(p)
          case s: SIG    => sigSerde.encode(s)
          case _         => Failure(Err("This shouldn't decode anything else"))
        },
      (vec: ByteVector) => pubkeySerde.decode(vec) orElse sigSerde.decode(vec)
    )

  implicit val e: Serde[CONSTANT] =
    Serde(
      discriminated[CONSTANT]
        .by(uint8)
        .typecase(20, pubkeyHashSerde.codec)
        .typecase(33, pubkeySerde.codec)
        .typecase(65, sigOrPubKeySerde.codec)
    )

}

sealed trait ScriptByteCode extends Product with Serializable { self =>

  def ++(b: ScriptByteCode): ScriptByteCode = {
    @annotation.tailrec
    def go(a: ScriptByteCode): ScriptByteCode =
      a match {
        case Append(s, End) => Append(s, b)
        case Append(_, t)   => go(t)
        case End            => b
      }

    b match {
      case End       => self
      case _: Append => go(self)
    }
  }
  def append(newS: Script, a: ScriptByteCode): ScriptByteCode =
    a match {
      case End          => Append(newS, End)
      case Append(s, t) => Append(s, append(newS, t))
    }

  def >>(s2: Script): ScriptByteCode =
    s2 match {
      case c: CONSTANT => append(OP_PUSHDATA1(c), self)
      case _: Script   => append(s2, self)
    }

  def mapL[A](f: Script => A): List[A] = {
    @annotation.tailrec
    def go(n: ScriptByteCode, acc: List[A]): List[A] =
      n match {
        case End          => acc
        case Append(s, t) => go(t, acc :+ f(s))
      }

    go(self, List.empty)
  }

}

case object End                                 extends ScriptByteCode
case class Append(s: Script, t: ScriptByteCode) extends ScriptByteCode

object ScriptByteCode {

  implicit val scriptByteCodeShow = new Show[ScriptByteCode] {
    def parse(s: String): Result[ScriptByteCode] = Predef.???

    def show(a: ScriptByteCode): String =
      a.mapL {
        case OP_PUSHDATA1(c) =>
          s"${c.getClass.getSimpleName}(${c.bytes.head.toInt}, ${c.bytes.tail.toHex})"
        case s => s.getClass.getSimpleName.init
      }.mkString(" >> ")
  }

}

sealed trait ScriptProgram
case class P2PKHScriptPubkey(pubkey: PublicKey)                                    extends ScriptProgram
case class P2PKhScriptSig(pubkey: PublicKey, signature: Signature, hashType: Byte) extends ScriptProgram

object ScriptCompiler {
  def compile(script: ScriptProgram): ScriptByteCode =
    script match {
      case P2PKHScriptPubkey(pubkey) =>
        OP_DUP >> OP_HASH160 >> PUBKEYHASH(pubkey.hash[Hash160]) >> OP_EQUALVERIFY >> OP_CHECKSIG
      case P2PKhScriptSig(pubkey, sig, hashType) =>
        SIG(sig, hashType) >> PUBKEY(pubkey)
    }

}
