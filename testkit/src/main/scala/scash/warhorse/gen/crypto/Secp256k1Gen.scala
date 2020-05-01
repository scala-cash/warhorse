package scash.warhorse.gen.crypto

import scash.warhorse.core._
import scash.warhorse.core.crypto.{ PrivateKey, PublicKey }

import zio.test.Gen

trait Secp256k1Gen {

  def privKey: Gen[Any, PrivateKey] = Gen.const(crypto.genPrivkey.require)

  def pubkey: Gen[Any, PublicKey] = keyPair.map(_._2)

  def pubKey(p: PrivateKey): Gen[Any, PublicKey] = Gen.const(crypto.genPubkey(p).require)

  def keyPair: Gen[Any, (PrivateKey, PublicKey)] =
    for {
      priv <- privKey
      pub  <- pubKey(priv)
    } yield (priv, pub)

}
