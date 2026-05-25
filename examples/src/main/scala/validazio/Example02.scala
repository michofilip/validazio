package validazio

import validazio.*
import zio.*

object Example02 extends ZIOAppDefault {

  case class Foo(
      field1: Option[String] = None,
      field2: Option[String] = None,
      field3: Option[String] = None,
  )

  case class FooValid(
      field1: String,
      field2: String,
      field3: Option[String],
  )

  given Validator[Foo, FooValid] = {
    val field1Validator: Validator[Foo, String] = labeled("field1") {
      required << minLength(3)
    }.contraMap[Foo](_.field1)

    val field2Validator: Validator[Foo, String] = id[Foo].map(_.field2) >> labeled("field2") {
      required << minLength(3)
    }

    val field3Validator: Validator[Foo, Option[String]] = labeled("field3") {
      id << minLength(3).optional
    }.contraMap[Foo](_.field3)

    (
      field1Validator
        ++ field2Validator
        ++ field3Validator
    ).map(FooValid.apply)
  }

  override def run: ZIO[ZIOAppArgs & Scope, Any, Any] = {
    val foo = Foo(
      field1 = Some("foo"),
      field2 = Some("foo"),
      field3 = Some("foo"),
    )

    for {
      fooValid <- Validator.validateZIO(ValidationException.apply)(foo).exit
      _        <- ZIO.log(s"fooValid: $fooValid")
    } yield ()
  }
}
