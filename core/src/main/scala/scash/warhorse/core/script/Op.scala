package scash.warhorse.core.script

object Op {

  //CONSTANT
  val False     = 0
  val Data      = 75
  val PushData1 = 76
  val True      = 81

  //STACK
  val Dup = 118

  //ARITHMETIC
  val EqualVerify = 136

  //CRYPTO
  val Hash160  = 169
  val CheckSig = 172

}
