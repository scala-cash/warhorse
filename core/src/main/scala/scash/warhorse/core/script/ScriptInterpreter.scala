package scash.warhorse.core.script

import scash.warhorse.{ Err, Result }
import scash.warhorse.Result.{ Failure, Successful }
import scash.warhorse.core._

import scash.warhorse.core.crypto.hash.Hash160

object ScriptInterpreter {

  sealed trait ExitCode
  case object Done extends ExitCode

  sealed trait Fail extends ExitCode

  case object EmptyStack    extends Fail
  case object NonEmptyStack extends Fail
  case object UnknownErr    extends Fail

  @annotation.tailrec
  def loop(script: ScriptByteCode, stack: Stack): ExitCode =
    script match {
      case Append(data: OP_PUSHDATA1, next) => loop(next, stack.push(data.c))
      case Append(op: OP_CODE, next) =>
        val newStack = interpret(op, stack)
        if (newStack.isSuccessful) loop(next, newStack.require) else EmptyStack
      case End => if (stack.isEmpty) Done else NonEmptyStack
      case _   => UnknownErr
    }

  def run(
    byteCode: ScriptByteCode
  ): Result[ExitCode] =
    loop(byteCode, Empty) match {
      case Done       => Successful(Done)
      case fail: Fail => Failure(Err(fail.getClass.getSimpleName))
    }

  def interpret(op: OP_CODE, stack: Stack): Result[Stack] = {
    val ans = op match {
      case OP_DUP     => stack.peek.map(stack.push)
      case OP_HASH160 => stack.pop.map(s => s._2.push(PUBKEYHASH(s._1.bytes.tail.hash[Hash160])))
      case OP_EQUALVERIFY =>
        stack.pop2.flatMap(s =>
          if (s._1 == s._2) Successful(s._3) else Failure(Err(s"${s._1} is not equal to ${s._2}"))
        )
      case OP_CHECKSIG => stack.pop2.map(_._3)
    }
    ans.mapErr { err =>
      Predef.println(err)
      err
    }
  }

  sealed trait Stack extends Product with Serializable { self =>

    def isEmpty = func(_ => true, Err("is empty")).getOrElse(false)

    def peek: Result[CONSTANT] = func(_.t, Err("Stack is empty"))

    def pop2: Result[(CONSTANT, CONSTANT, Stack)] =
      for {
        a <- pop
        b <- a._2.pop
      } yield (a._1, b._1, b._2)

    def pop: Result[(CONSTANT, Stack)] = func(c => (c.t, c.s), Err("Stack is empty"))

    def func[B](f: Cons => B, err: Err): Result[B] =
      self match {
        case Empty   => Failure(err)
        case c: Cons => Successful(f(c))
      }

    def push(a: CONSTANT): Stack = Cons(a, self)
  }

  case object Empty                      extends Stack
  case class Cons(t: CONSTANT, s: Stack) extends Stack

  object Stack {
    def apply(s: CONSTANT*): Stack = apply(s.toList)

    def apply(s: List[CONSTANT]): Stack =
      s match {
        case h :: t => Cons(h, apply(t))
        case Nil    => Empty
      }
  }
}
