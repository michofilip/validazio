package validazio

import validazio.Validator
import validazio.Validator.*
import zio.*

object Example01 extends ZIOAppDefault {

  case class Foo(
      field1: Option[String] = None,
      field2: Option[String] = None,
      field3: Option[Long] = None,
  )

  case class FooValid(
      field1: String,
      field2: Option[String],
      field3: Long,
  )

  given Validator[Foo, FooValid] = {
    val nameValidator: Validator[Foo, String] = labeled("field1") {
      required << all(
        notBlank,
        minLength(3),
        regExr("[a-z]", "must contain a lowercase character"),
        regExr("[A-Z]", "must contain an uppercase character"),
        regExr("[0-9]", "must contain a digit"),
      )
    }.contraMap[Foo](_.field1)

    val descriptionValidator: Validator[Foo, Option[String]] = labeled("field2") {
      id << all(notBlank, minLength(1)).optional
    }.contraMap[Foo](_.field2)

    val numberValidator: Validator[Foo, Long] = labeled("field3") {
      required << min(18L)
    }.contraMap[Foo](_.field3)

    validateWith(
      nameValidator,
      descriptionValidator,
      numberValidator,
    )(FooValid.apply)
  }

  override def run: ZIO[ZIOAppArgs & Scope, Any, Any] = {
    val foo = Foo(
      field1 = Some("aA1"),
      field2 = Some("foo"),
      field3 = Some(19L),
    )

    for {
      fooValid <- Validator.validateZIO(ValidationException.apply)(foo).exit
      _        <- ZIO.log(s"fooValid: $fooValid")
    } yield ()
  }
}
