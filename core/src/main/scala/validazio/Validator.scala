package validazio

import validazio.Validator.*
import zio.*
import zio.prelude.*

trait Validator[In, Out] {
  def validate(value: In): Validation[String, Out]

  final def optional: Validator[Option[In], Option[Out]] =
    ValidateForEach(this)

  final def list: Validator[List[In], List[Out]] =
    ValidateForEach(this)

  final def map[Out2](f: Out => Out2): Validator[In, Out2] =
    Map(this, f)

  final def contraMap[In0](f: In0 => In): Validator[In0, Out] =
    ContraMap(this, f)

  final def flatMap[Out2](validator: Validator[Out, Out2]): Validator[In, Out2] =
    FlatMap(this, validator)

  final def >>[Out2](validator: Validator[Out, Out2]): Validator[In, Out2] =
    flatMap(validator)

  final def tap[Out2](validator: Validator[Out, Out2]): Validator[In, Out] =
    Tap(this, validator)

  final def <<[Out2](validator: Validator[Out, Out2]): Validator[In, Out] =
    tap(validator)

  final def unit: Validator[In, Unit] =
    map(_ => ())

  final def zip[Out2](
      validator: Validator[In, Out2],
  )(using zippable: Zippable[Out, Out2]): Validator[In, zippable.Out] =
    Zip(this, validator, zippable.zip)

  final def ++[Out2](
      validator: Validator[In, Out2],
  )(using zippable: Zippable[Out, Out2]): Validator[In, zippable.Out] =
    zip(validator)

  final def when(predicate: => Boolean): Validator[In, Option[Out]] =
    When(this, predicate)

  final def unless(predicate: => Boolean): Validator[In, Option[Out]] =
    when(!predicate)
}

object Validator {

  private[validazio] case class Id[T]() extends Validator[T, T] {
    override def validate(value: T): Validation[String, T] =
      Validation.succeed(value)
  }

  private[validazio] case class Required[T](label: String) extends Validator[Option[T], T] {
    override def validate(value: Option[T]): Validation[String, T] =
      Validation.fromOptionWith(s"$label is required")(value)
  }

  private[validazio] case class Condition[T](predicate: T => Boolean, description: String) extends Validator[T, T] {
    override def validate(value: T): Validation[String, T] = {
      Validation.fromPredicateWith(description)(value)(predicate)
    }
  }

  private[validazio] case class ValidateAll[In, Out, F[+_]: ForEach](private val validators: F[Validator[In, Out]])
      extends Validator[In, F[Out]] {
    override def validate(value: In): Validation[String, F[Out]] =
      Validation.validateAll(validators.map(_.validate(value)))
  }

  private[validazio] case class ValidateForEach[In, Out, F[+_]: ForEach](private val validator: Validator[In, Out])
      extends Validator[F[In], F[Out]] {
    override def validate(value: F[In]): Validation[String, F[Out]] =
      Validation.validateAll(value.map(validator.validate))
  }

  private[validazio] case class Tap[In, Out, Out2](
      private val validator1: Validator[In, Out],
      private val validator2: Validator[Out, Out2],
  ) extends Validator[In, Out] {
    override def validate(value: In): Validation[String, Out] = {
      validator1.validate(value).tap(validator2.validate)
    }
  }

  private[validazio] case class Map[In, Out, Out2](
      private val validator: Validator[In, Out],
      private val f: Out => Out2,
  ) extends Validator[In, Out2] {
    override def validate(value: In): Validation[String, Out2] =
      validator.validate(value).map(f)
  }

  private[validazio] case class ContraMap[In, In2, Out](
      private val validator: Validator[In2, Out],
      private val f: In => In2,
  ) extends Validator[In, Out] {
    override def validate(value: In): Validation[String, Out] =
      validator.validate(f(value))
  }

  private[validazio] case class FlatMap[In, Out, Out2](
      private val validator1: Validator[In, Out],
      private val validator2: Validator[Out, Out2],
  ) extends Validator[In, Out2] {
    override def validate(value: In): Validation[String, Out2] = {
      validator1.validate(value).flatMap(validator2.validate)
    }
  }

  private[validazio] case class Zip[In, Out1, Out2, Out](
      private val validator1: Validator[In, Out1],
      private val validator2: Validator[In, Out2],
      private val f: (Out1, Out2) => Out,
  ) extends Validator[In, Out] {
    override def validate(value: In): Validation[String, Out] = {
      validator1.validate(value).zipWithPar(validator2.validate(value))(f)
    }
  }

  private[validazio] case class When[In, Out](
      private val validator: Validator[In, Out],
      private val predicate: Boolean,
  ) extends Validator[In, Option[Out]] {
    override def validate(value: In): Validation[String, Option[Out]] =
      if (predicate) validator.validate(value).map(Some.apply) else Validation.succeed(None)
  }

}
