package scash.warhorse.core.crypto

case class ECDSA()

object ECDSA {
  implicit val ecdsaSigner: Signer[ECDSA] = Predef.???
}
