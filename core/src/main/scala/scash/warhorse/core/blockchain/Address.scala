package scash.warhorse.core.blockchain

import scash.warhorse.Result
import scash.warhorse.core.crypto.PublicKey
import scash.warhorse.core.crypto.hash.Hash160
import scash.warhorse.core._
import scodec.bits.ByteVector

sealed trait Address extends Product with Serializable { self =>
  def toCashAddr: String   = CashAddr.addrShow.show(self)
  def toLegacyAddr: String = LegacyAddr.addrShow.show(self)
}

case class P2PKH(net: Net, pubKeyHash: Hash160)      extends Address
case class P2SH(net: Net, redeemScriptHash: Hash160) extends Address

object Address {

  def p2pkh(net: Net, pubKey: PublicKey): P2PKH =
    P2PKH(net, pubKey.hash[Hash160])

  def p2sh(net: Net, redeemScript: ByteVector): P2SH =
    P2SH(net, redeemScript.hash[Hash160])

  def apply(addr: String): Result[Address] =
    LegacyAddr.addrShow.parse(addr) orElse CashAddr.addrShow.parse(addr)

  def cashAddrToLegacy(addr: String): Result[String] =
    CashAddr.addrShow
      .parse(addr)
      .map(LegacyAddr.addrShow.show)

  def legacyToCashAddr(addr: String): Result[String] =
    LegacyAddr.addrShow
      .parse(addr)
      .map(CashAddr.addrShow.show)
}
