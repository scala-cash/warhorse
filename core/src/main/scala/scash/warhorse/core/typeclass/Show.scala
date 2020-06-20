package scash.warhorse.core.typeclass

import scash.warhorse.{ Err, Result }
import scash.warhorse.Result.{ Failure, Successful }

trait Show[A] {
  def show(a: A): String

  def parse(s: String): Result[A]
}

object Show {
  def apply[A](implicit c: Show[A]): Show[A] = c

  def constant[A](str: String, a: A): Show[A] =
    new Show[A] {
      def show(a: A): String = str
      def parse(s: String) =
        if (s == str) Successful(a)
        else Failure(Err(s"$s does not match $str"))

    }
}

trait ShowSyntax {
  implicit class ShowSyntaxOps[A: Show](a: A) {
    def show: String = Show[A].show(a)
  }
}
