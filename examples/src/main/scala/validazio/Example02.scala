package validazio

import validazio.Example01.{Foo, FooValid}
import validazio.Validator
import validazio.Validator.*
import zio.*

object Example02 extends ZIOAppDefault {

  case class Bar(
      field1: Option[Foo] = None,
      field2: Option[Foo] = None,
      field3: Option[List[Foo]] = None,
  )

  case class BarValid(
      field1: FooValid,
      field2: Option[FooValid],
      field3: List[FooValid],
  )

  given Validator[Bar, BarValid] = {
    val field1Validator: Validator[Bar, FooValid] = labeled("field1") {
      required >> valid[Foo, FooValid]
    }.contraMap[Bar](_.field1)

    val field2Validator: Validator[Bar, Option[FooValid]] = labeled("field2") {
      valid[Foo, FooValid].optional
    }.contraMap[Bar](_.field2)

    val field3Validator: Validator[Bar, List[FooValid]] = labeled("field3") {
      valid[Foo, FooValid].list.optional.map(_.getOrElse(List.empty))
    }.contraMap[Bar](_.field3)

    validateWith(
      field1Validator,
      field2Validator,
      field3Validator,
    )(BarValid.apply)
  }

  override def run: ZIO[ZIOAppArgs & Scope, Any, Any] = {
    val foo1 = Foo(field1 = Some("aA1"), field2 = Some("a1"), field3 = Some(18))
    val foo2 = Foo(field1 = Some("aA1"), field2 = Some("a1"), field3 = Some(18))
    val foo3 = Foo(field1 = Some("aA1"), field2 = Some("a1"), field3 = Some(18))
    val foo4 = Foo(field1 = Some("aA1"), field2 = Some("a1"), field3 = Some(18))
    val bar  = Bar(
      field1 = Some(foo1),
      field2 = Some(foo2),
      field3 = Some(List(foo3, foo4)),
    )

    for {
      barValid <- Validator.validateZIO(ValidationException.apply)(bar).exit
      _        <- ZIO.log(s"barValid: $barValid")
    } yield ()
  }
}
