package scash.warhorse.core.typeclass

import scash.warhorse.Result

trait Cipher[A] {
  def encode(a: A): String

  def decode(s: String): Result[A]
}

object Cipher   {
  def apply[A](implicit c: Cipher[A]) = c
}
