package scash.warhorse.gen.crypto

import scash.warhorse.core._
import scash.warhorse.core.crypto.{ PrivateKey, PublicKey, Schnorr, Signature, Signer }
import scash.warhorse.gen

import zio.random
import zio.test.{ Gen, Sized }
import zio.random.Random

trait Secp256k1Gen {

  def sigAndPubkey: Gen[Random with Sized, (Signature, PublicKey)] =
    for {
      p    <- keyPair
      msg  <- gen.randomMessage
      hash <- gen.sha256Bytes(msg)
    } yield (Signer[Schnorr].sign(hash, p._1).require, p._2)

  def privKey: Gen[Any, PrivateKey] = Gen.const(crypto.genPrivkey.require)

  def pubkey: Gen[random.Random, PublicKey] = keyPair.map(_._2)

  def pubKey(p: PrivateKey): Gen[Any, PublicKey] = Gen.const(crypto.genPubkey(p).require)

  def pubKeyCompressed(p: PrivateKey): Gen[Any, PublicKey] = Gen.const(crypto.genPubkeyCompressed(p).require)

  def keyPair: Gen[random.Random, (PrivateKey, PublicKey)] =
    for {
      priv <- privKey
      pub  <- Gen.oneOf(pubKeyCompressed(priv), pubKey(priv))
    } yield (priv, pub)

}
