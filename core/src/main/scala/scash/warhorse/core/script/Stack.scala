package scash.warhorse.core.script

import scash.warhorse.{ Err, Result }
import scash.warhorse.Result.{ Failure, Successful }

sealed trait Stack extends Product with Serializable { self =>

  def isEmpty = func(_ => false, Err("is empty")).getOrElse(true)

  def peek: Result[Constant] = func(_.t, Err("Stack is empty"))

  def pop2: Result[(Constant, Constant, Stack)] =
    for {
      a <- pop
      b <- a._2.pop
    } yield (a._1, b._1, b._2)

  def pop: Result[(Constant, Stack)] = func(c => (c.t, c.s), Err("Stack is empty"))

  def func[B](f: Cons => B, err: Err): Result[B] =
    self match {
      case Empty   => Failure(err)
      case c: Cons => Successful(f(c))
    }

  def push(a: Constant): Stack = Cons(a, self)
}

case object Empty                      extends Stack
case class Cons(t: Constant, s: Stack) extends Stack

object Stack {
  def apply(s: Constant*): Stack = apply(s.toList)

  def apply(s: List[Constant]): Stack =
    s match {
      case h :: t => Cons(h, apply(t))
      case Nil    => Empty
    }
}
