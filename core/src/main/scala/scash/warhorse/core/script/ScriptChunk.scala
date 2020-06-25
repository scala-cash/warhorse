package scash.warhorse.core.script

import scash.warhorse.{ Err, Result }
import scash.warhorse.core.typeclass.{ ScriptByteCode, Serde, Show }
import zio.Chunk

import Predef.augmentString
import scala.util.Try

final case class ScriptChunk(private val c: Chunk[Script]) { self =>
  def ++(s2: ScriptChunk) = ScriptChunk(c ++ s2.c)

  def >>(s: Script): ScriptChunk = ScriptChunk(c.appended(s))

  def map[B](f: Script => B): Chunk[B] = c.map(f)

  def to[A: ScriptByteCode]: Result[A] = ScriptByteCode[A].fromASM(self)

  def foldWhile[B](b: B)(pred: B => Boolean)(f: (B, Script) => B): B = c.foldWhile(b)(pred)(f)

  def head: Result[Script] = Result.fromOption(c.headOption, Err("Stack is empty"))

  def at(i: Int): Result[Script] = Result.fromTry(Try(c(i)))
}

object ScriptChunk {
  def empty: ScriptChunk = ScriptChunk(Chunk.empty)

  def apply(s: Script*): ScriptChunk = ScriptChunk(Chunk(s: _*))
  import scodec.codecs._
  implicit val scriptChunkSerde =
    Serde(vector(Script.scriptSerde.codec))
      .xmap[ScriptChunk](l => ScriptChunk(Chunk.fromIterable(l)), _.c.toVector)

  implicit val scriptChunkShow = new Show[ScriptChunk] {
    def parse(s: String): Result[ScriptChunk] = Predef.???

    def show(a: ScriptChunk): String =
      a.map {
        case c: Constant => s"${c.getClass.getSimpleName}(${c.bytes.head.toInt}, ${c.bytes.tail.toHex})"
        case s           => s.getClass.getSimpleName.init
      } mkString " >> "
  }
}
