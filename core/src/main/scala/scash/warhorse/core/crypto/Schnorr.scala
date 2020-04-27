package scash.warhorse.core.crypto

case class Schnorr()

object Schnorr {
  implicit val schnorrSigner: Signer[Schnorr] = Predef.???
}
