package validazio

import validazio.Validator.*
import zio.*
import zio.prelude.*

sealed trait Validator[In, Out] {
  def validate(value: In): Validation[ValidationFailure, Out]

  final def map[Out2](f: Out => Out2): Validator[In, Out2] =
    MapValidator(this, f)

  final def contraMap[In0](f: In0 => In): Validator[In0, Out] =
    ContraMapValidator(this, f)

  final def flatMap[Out2](validator: Validator[Out, Out2]): Validator[In, Out2] =
    FlatMapValidator(this, validator)

  final def >>[Out2](validator: Validator[Out, Out2]): Validator[In, Out2] =
    flatMap(validator)

  final def tap[Out2](validator: Validator[Out, Out2]): Validator[In, Out] =
    TapValidator(this, validator)

  final def <<[Out2](validator: Validator[Out, Out2]): Validator[In, Out] =
    tap(validator)

  final def optional: Validator[Option[In], Option[Out]] =
    ForEachValidator(this)

  final def list: Validator[List[In], List[Out]] =
    ForEachValidator(this)

  final def set: Validator[Set[In], Set[Out]] =
    list.map(_.toSet).contraMap(_.toList)

  final def keyValuePairs[K]: Validator[Map[K, In], Map[K, Out]] = {
    val keys   = id[Map[K, In]].map(_.keys)
    val values = list.contraMap[Map[K, In]](_.values.toList)

    keys.zip(values).map { case (k, v) => k.zip(v).toMap }
  }

  final def unit: Validator[In, Unit] =
    map(_ => ())

  final def zip[Out2](
      validator: Validator[In, Out2],
  )(using zippable: Zippable[Out, Out2]): Validator[In, zippable.Out] =
    ZipValidator(this, validator, zippable.zip)

  final def ++[Out2](
      validator: Validator[In, Out2],
  )(using zippable: Zippable[Out, Out2]): Validator[In, zippable.Out] =
    zip(validator)

  final def when(predicate: => Boolean): Validator[In, Option[Out]] =
    WhenValidator(this, predicate)

  final def unless(predicate: => Boolean): Validator[In, Option[Out]] =
    when(!predicate)
}

object Validator {

  private[validazio] case class IdValidator[T]() extends Validator[T, T] {
    override def validate(value: T): Validation[ValidationFailure, T] =
      Validation.succeed(value)
  }

  private[validazio] case class RequiredValidator[T](label: Label) extends Validator[Option[T], T] {
    override def validate(value: Option[T]): Validation[ValidationFailure, T] =
      Validation.fromOptionWith(ValidationFailure.Required(label))(value)
  }

  private[validazio] case class ConditionValidator[T](predicate: T => Boolean, validationFailure: ValidationFailure)
      extends Validator[T, T] {
    override def validate(value: T): Validation[ValidationFailure, T] = {
      Validation.fromPredicateWith(validationFailure)(value)(predicate)
    }
  }

  private[validazio] case class AllValidator[In, Out, F[+_]: ForEach](private val validators: F[Validator[In, Out]])
      extends Validator[In, F[Out]] {
    override def validate(value: In): Validation[ValidationFailure, F[Out]] =
      Validation.validateAll(validators.map(_.validate(value)))
  }

  private[validazio] case class ForEachValidator[In, Out, F[+_]: ForEach](private val validator: Validator[In, Out])
      extends Validator[F[In], F[Out]] {
    override def validate(value: F[In]): Validation[ValidationFailure, F[Out]] =
      Validation.validateAll(value.map(validator.validate))
  }

  private[validazio] case class TapValidator[In, Out, Out2](
      private val validator1: Validator[In, Out],
      private val validator2: Validator[Out, Out2],
  ) extends Validator[In, Out] {
    override def validate(value: In): Validation[ValidationFailure, Out] = {
      validator1.validate(value).tap(validator2.validate)
    }
  }

  private[validazio] case class MapValidator[In, Out, Out2](
      private val validator: Validator[In, Out],
      private val f: Out => Out2,
  ) extends Validator[In, Out2] {
    override def validate(value: In): Validation[ValidationFailure, Out2] =
      validator.validate(value).map(f)
  }

  private[validazio] case class ContraMapValidator[In, In2, Out](
      private val validator: Validator[In2, Out],
      private val f: In => In2,
  ) extends Validator[In, Out] {
    override def validate(value: In): Validation[ValidationFailure, Out] =
      validator.validate(f(value))
  }

  private[validazio] case class FlatMapValidator[In, Out, Out2](
      private val validator1: Validator[In, Out],
      private val validator2: Validator[Out, Out2],
  ) extends Validator[In, Out2] {
    override def validate(value: In): Validation[ValidationFailure, Out2] = {
      validator1.validate(value).flatMap(validator2.validate)
    }
  }

  private[validazio] case class ZipValidator[In, Out1, Out2, Out](
      private val validator1: Validator[In, Out1],
      private val validator2: Validator[In, Out2],
      private val f: (Out1, Out2) => Out,
  ) extends Validator[In, Out] {
    override def validate(value: In): Validation[ValidationFailure, Out] = {
      validator1.validate(value).zipWithPar(validator2.validate(value))(f)
    }
  }

  private[validazio] case class WhenValidator[In, Out](
      private val validator: Validator[In, Out],
      private val predicate: Boolean,
  ) extends Validator[In, Option[Out]] {
    override def validate(value: In): Validation[ValidationFailure, Option[Out]] =
      if (predicate) validator.validate(value).map(Some.apply) else Validation.succeed(None)
  }

}
