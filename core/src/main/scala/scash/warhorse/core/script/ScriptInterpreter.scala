package scash.warhorse.core.script

import scash.warhorse.{ Err, Result }
import scash.warhorse.Result.{ Failure, Successful }
import scash.warhorse.core.script.ScriptByteCode._

object ScriptInterpreter {

  sealed trait ExitCode
  case object Done extends ExitCode

  sealed trait Fail extends ExitCode

  case object EmptyStack    extends Fail
  case object NonEmptyStack extends Fail
  case object UnknownErr    extends Fail

  def loop(script: ScriptByteCode, stack: Stack): ExitCode =
    script match {
      case Line(data: Constant, next) => loop(next, stack.push(data))
      case Line(op: Operation, next) =>
        interpret(op, stack).fold(_ => EmptyStack, loop(next, _))
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

  def interpret(op: Operation, stack: Stack): Result[Stack] = {
    val ans = op match {
      case OP_DUP         => StackInterpreter.opDup(stack)
      case OP_HASH160     => CryptoInterpreter.opHash160(stack)
      case OP_EQUALVERIFY => CryptoInterpreter.opEqualVerify(stack)
      case OP_CHECKSIG    => CryptoInterpreter.opCheckSig(stack)
    }
    ans.mapErr { err =>
      Predef.println(err)
      err
    }
  }
}
