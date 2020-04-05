package scash.warhorse.core.typeclass

trait CNumeric[A] {
  def andMask: BigInt
  def num: A => BigInt
  def apply: BigInt => A

  def sum[A1: CNumeric](a: A, a1: A1): A = apply(num(a) + CNumeric[A1].num(a1))
  def sub[A1: CNumeric](a: A, a1: A1): A = apply(num(a) - CNumeric[A1].num(a1))
  def mul[A1: CNumeric](a: A, a1: A1): A = apply(num(a) * CNumeric[A1].num(a1))

  def gt[A1: CNumeric](a: A, a1: A1): Boolean  = num(a) > CNumeric[A1].num(a1)
  def gte[A1: CNumeric](a: A, a1: A1): Boolean = num(a) >= CNumeric[A1].num(a1)
  def lt[A1: CNumeric](a: A, a1: A1): Boolean  = num(a) < CNumeric[A1].num(a1)
  def lte[A1: CNumeric](a: A, a1: A1): Boolean = num(a) <= CNumeric[A1].num(a1)

  def or[A1: CNumeric](a: A, a1: A1): A  = apply(num(a) | CNumeric[A1].num(a1))
  def and[A1: CNumeric](a: A, a1: A1): A = apply(num(a) & CNumeric[A1].num(a1))
  def xor[A1: CNumeric](a: A, a1: A1): A = apply(num(a) ^ CNumeric[A1].num(a1))

  def negative(a: A): A = apply(-num(a))

  def shiftL(a: A, a1: Int): A = apply((num(a) << a1) & andMask)

  //this check is for weird behavior with the jvm and shift rights
  //https://stackoverflow.com/questions/47519140/bitwise-shift-right-with-long-not-equaling-zero/47519728#47519728
  def shiftR(a: A, a1: Int): A =
    if (a1.toLong > 63) apply(0)
    else apply((num(a) << a1) & andMask)
}

object CNumeric {
  def apply[A](implicit n: CNumeric[A]): CNumeric[A] = n

  def apply[A](mask: BigInt)(n: A => BigInt, app: BigInt => A): CNumeric[A] = new CNumeric[A] {
    def andMask = mask
    def num     = n
    def apply   = app
  }

}

trait CNumericSyntax {
  implicit class CNumericSyntaxOps[A: CNumeric](a: A) {
    def +(num: A): A = CNumeric[A].sum(a, num)

    def add[A1: CNumeric](num: A1): A      = CNumeric[A].sum[A1](a, num)
    def -[A1: CNumeric](num: A1): A        = CNumeric[A].sub(a, num)
    def *[A1: CNumeric](num: A1): A        = CNumeric[A].mul(a, num)
    def >[A1: CNumeric](num: A1): Boolean  = CNumeric[A].gt(a, num)
    def >=[A1: CNumeric](num: A1): Boolean = CNumeric[A].gte(a, num)
    def <[A1: CNumeric](num: A1): Boolean  = CNumeric[A].lt(a, num)
    def <=[A1: CNumeric](num: A1): Boolean = CNumeric[A].lte(a, num)

    def <<(num: Int): A = CNumeric[A].shiftL(a, num)
    def >>(num: Int): A = CNumeric[A].shiftR(a, num)

    def |(num: A): A = CNumeric[A].or(a, num)
    def &(num: A): A = CNumeric[A].and(a, num)
    def ^(num: A): A = CNumeric[A].xor(a, num)
    def unary_- : A  = CNumeric[A].negative(a)
  }
}
