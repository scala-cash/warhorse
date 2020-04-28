package scash.warhorse

import scash.warhorse.core.typeclass.{ CNumeric, CNumericSyntax, SerdeSyntax }
import scash.warhorse.core.crypto.SignatureSyntax
package object core extends SerdeSyntax with CNumericSyntax with SignatureSyntax {
  import scala.{ Predef => P }

  implicit val bigIntNumeric: CNumeric[BigInt] =
    CNumeric[BigInt](
      0xFFFFFFFFFFFFFFFFL,
      BigInt(Long.MinValue),
      BigInt(Long.MaxValue)
    )(P.identity, P.identity)

  implicit val longNumeric: CNumeric[Long] =
    CNumeric[Long](
      0xFFFFFFFFFFFFFFFFL,
      Long.MinValue,
      Long.MaxValue
    )(P.identity[Long], _.toLong)

  implicit val intNumeric: CNumeric[Int] =
    CNumeric[Int](
      0xFFFFFFFF,
      Int.MinValue,
      Int.MaxValue
    )(P.identity[Int], _.toInt)

}
