package scash.warhorse.core.typeclass

import scash.warhorse.{ Err, Result }
import scash.warhorse.Result.{ Failure, Successful }

import Predef.augmentString

trait Show[A] {
  def show(a: A): String

  def parse(s: String): Result[A]
}

object Show {
  def apply[A](implicit c: Show[A]): Show[A] = c

  def apply[A](t: A): Show[A] =
    new Show[A] {
      def show(a: A): String          = a.getClass.getSimpleName.init
      def parse(s: String): Result[A] = if (show(t) == s) Successful(t) else Failure(Err("asdsad"))
    }
}

trait ShowSyntax {
  implicit class ShowSyntaxOps[A: Show](a: A) {
    def show: String = Show[A].show(a)
  }
}
