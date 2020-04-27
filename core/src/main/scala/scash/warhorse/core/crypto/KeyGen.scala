package scash.warhorse.core.crypto

trait KeyGen[A] {
  def genPrivkey: PrivateKey

  def genPubkey(publicKey: PrivateKey): PublicKey

  def isValidPubkey(pubkey: PublicKey): Boolean

  def isValidPrivatekey(pubkey: PrivateKey): Boolean
}
