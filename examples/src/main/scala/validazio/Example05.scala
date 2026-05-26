package validazio

import validazio.*
import zio.*

object Example05 extends ZIOAppDefault {

  val environment = "TEST"

  case class Foo(
      field1: Option[String] = None,
      field2: Option[String] = None,
  )

  private def fooValidator(environment: String): Validator[Foo, Foo] = {
    val field1Validator: Validator[Foo, Option[String]] = labeled("field1") {
      required.when(environment == "TEST")
    }.contraMap[Foo](_.field1)

    val field2Validator: Validator[Foo, Option[String]] = labeled("field2") {
      required.unless(environment == "PROD")
    }.contraMap[Foo](_.field2)

    id << allDiscard(
      field1Validator,
      field2Validator,
    )
  }

  given Validator[Foo, Foo] = fooValidator(environment)

  override def run: ZIO[ZIOAppArgs & Scope, Any, Any] = {
    val foo = Foo(
      field1 = Some("foo"),
      field2 = Some("foo"),
    )

    val fooIncorrect = Foo(
      field1 = None,
      field2 = None,
    )

    for {
      foo          <- validateZIO(ValidationException.apply)(foo).exit
      _            <- ZIO.log(s"foo: $foo")
      fooIncorrect <- validateZIO(ValidationException.apply)(fooIncorrect).exit
      _            <- ZIO.log(s"fooIncorrect: $fooIncorrect")
    } yield ()
  }
}
