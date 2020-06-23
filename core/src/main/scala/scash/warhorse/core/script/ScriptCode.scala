package scash.warhorse.core.script

import scash.warhorse.Result
import scash.warhorse.core.typeclass.Show

import Predef.augmentString

sealed trait ScriptByteCode extends Product with Serializable { self =>
  import ScriptByteCode._

  def ++(b: ScriptByteCode): ScriptByteCode = {
    def go(a: ScriptByteCode): ScriptByteCode =
      a match {
        case Line(s, t) => Line(s, go(t))
        case End        => b
      }

    b match {
      case End     => self
      case _: Line => go(self)
    }
  }
  def append(newS: Script, a: ScriptByteCode): ScriptByteCode =
    a match {
      case End        => Line(newS, End)
      case Line(s, t) => Line(s, append(newS, t))
    }

  def >>(s2: Script): ScriptByteCode = append(s2, self)

  def mapL[A](f: Script => A): List[A] = {
    @annotation.tailrec
    def go(n: ScriptByteCode, acc: List[A]): List[A] =
      n match {
        case End        => acc
        case Line(s, t) => go(t, acc :+ f(s))
      }

    go(self, List.empty)
  }

}

object ScriptByteCode {
  case object End                               extends ScriptByteCode
  case class Line(s: Script, t: ScriptByteCode) extends ScriptByteCode

  implicit val scriptByteCodeShow = new Show[ScriptByteCode] {
    def parse(s: String): Result[ScriptByteCode] = Predef.???

    def show(a: ScriptByteCode): String =
      a.mapL {
        case c: Constant =>
          s"${c.getClass.getSimpleName}(${c.bytes.head.toInt}, ${c.bytes.tail.toHex})"
        case s => s.getClass.getSimpleName.init
      }.mkString(" >> ")
  }

}
