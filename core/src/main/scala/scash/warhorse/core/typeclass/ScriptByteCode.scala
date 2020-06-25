package scash.warhorse.core.typeclass

import scash.warhorse.Result
import scash.warhorse.core.script.ScriptChunk
import scodec.bits.ByteVector

trait ScriptByteCode[A] {
  def toASM(a: A): ScriptChunk

  def fromASM(scriptChunk: ScriptChunk): Result[A]
}

object ScriptByteCode {

  def apply[A](implicit s: ScriptByteCode[A]) = s
}

trait ScriptByteCodeSyntax {
  implicit def toSerde[A](implicit sb: ScriptByteCode[A]): Serde[A] =
    ScriptChunk.scriptChunkSerde.narrow[A](s => sb.fromASM(s), a => sb.toASM(a))

  implicit class ScriptByteCodeSyntaxOps[A: ScriptByteCode](private val a: A) {
    def toASM: ScriptChunk             = ScriptByteCode[A].toASM(a)
    def toASMbytes: Result[ByteVector] = Serde[ScriptChunk].encode(toASM)
  }
}
