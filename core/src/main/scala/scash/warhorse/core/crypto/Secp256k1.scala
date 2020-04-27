package scash.warhorse.core.crypto

case class Secp256k1()

object Secp256k1 {
  implicit val secp256k1KeyGen: KeyGen[Secp256k1] = Predef.???
}
