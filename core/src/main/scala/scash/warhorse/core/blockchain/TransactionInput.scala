package scash.warhorse.core.blockchain

import scash.warhorse.core.number.Uint32
import scash.warhorse.core.typeclass.Serde

import scodec.bits._

case class TransactionInput(
  previousOutput: TransactionOutPoint,
  sigScript: ByteVector, //TODO: change when implenting Script
  sequence: Uint32
)

object TransactionInput {
  implicit val transactionInputSerde: Serde[TransactionInput] = Serde(
    (
      Serde[TransactionOutPoint].codec ::
        CompactSizeUint.bytes ::
        Serde[Uint32].codec
    ).as[TransactionInput]
  )
}
