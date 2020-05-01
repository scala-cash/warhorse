package scash.warhorse

import scash.warhorse.Result.{ Failure, Successful }
import scash.warhorse.Err.ParseError

import scala.io.{ BufferedSource, Source }

import io.circe.parser._
import io.circe._

import zio.{ Managed, Task, ZIO }
import zio.test.Assertion
import zio.test.Assertion.Render.param
import zio.test.Assertion.isSubtype

object util {
  def success[A](expected: A): Assertion[Result[A]] =
    Assertion.assertion("success")(param(expected))(_ == Successful(expected))

  def successResult[A](expected: Result[A]) = success[A](expected.require)

  def failure = isSubtype[Failure](Assertion.anything)

  def success[A]() = isSubtype[Successful[A]](Assertion.anything)

  def openFile(fileName: String): Managed[Throwable, BufferedSource] =
    Managed.makeEffect(Source.fromResource(fileName))(_.close())

  def parseJson[A: Decoder](js: String): Result[A] =
    decode[A](js).fold(e => Failure(ParseError("JsonParser", e.getMessage)), Successful(_))

  def parseJsonfromFile[A: Decoder](fileName: String): Task[Result[A]] =
    openFile(fileName)
      .map(b => parseJson[A](b.getLines().mkString))
      .use(ZIO.succeed(_))

  type CSV = List[List[String]]
  implicit val csvDecoder: Decoder[CSV] = Decoder.decodeList(Decoder.decodeList[String])
}
