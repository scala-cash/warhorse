package scash.warhorse.core.script

import scodec.codecs._

sealed trait Operation     extends Script
case object OP_DUP         extends Operation
case object OP_HASH160     extends Operation
case object OP_EQUALVERIFY extends Operation
case object OP_CHECKSIG    extends Operation

object OP_CODE {

  implicit val opsCodec =
    discriminated[Operation]
      .by(uint8)
      .typecase(Op.Dup, provide(OP_DUP))
      .typecase(Op.EqualVerify, provide(OP_EQUALVERIFY))
      .typecase(Op.Hash160, provide(OP_HASH160))
      .typecase(Op.CheckSig, provide(OP_CHECKSIG))
}
