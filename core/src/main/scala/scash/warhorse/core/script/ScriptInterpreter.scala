package scash.warhorse.core.script

import scash.warhorse.{ Err, Result }
import scash.warhorse.Result.{ Failure, Successful }

object ScriptInterpreter {

  sealed trait ExitCode
  case object Done            extends ExitCode
  case class FailOp(err: Err) extends ExitCode
  case object NonEmptyStack   extends ExitCode

  def run(s: ScriptChunk): ExitCode =
    s.foldWhile[Result[Stack]](Successful(Empty))(_.isSuccessful) {
        case (Successful(stack), con: Constant) => Successful(stack.push(con))
        case (Successful(stack), op: Operation) => interpret(op, stack)
        case _                                  => Failure(Err("This state should not be possible"))
      }
      .fold(err => FailOp(err), s => if (s.isEmpty) Done else NonEmptyStack)

  def interpret(op: Operation, stack: Stack): Result[Stack] =
    op match {
      case OP_DUP         => StackInterpreter.opDup(stack)
      case OP_HASH160     => CryptoInterpreter.opHash160(stack)
      case OP_EQUALVERIFY => CryptoInterpreter.opEqualVerify(stack)
      case OP_CHECKSIG    => CryptoInterpreter.opCheckSig(stack)
    }
}
