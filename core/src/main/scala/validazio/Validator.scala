package validazio

import validazio.Validator.*
import zio.*
import zio.prelude.*

export Validator.*

trait Validator[In, Out] { self =>
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
    this.zip(validator)
}

object Validator {
  def validateZIO[In, Err: Associative, Out](f: String => Err)(value: In): Validator[In, Out] ?=> IO[Err, Out] = {
    valid.validate(value).mapError(f).toZIOAssociative
  }

  def labeled[In, Out](label: String)(validator: Label ?=> Validator[In, Out]): Validator[In, Out] = {
    validator(using Label(label))
  }

  def id[T]: Validator[T, T] =
    Id()

  def required[T]: Label ?=> Validator[Option[T], T] =
    Required(summon[Label].label)

  def condition[T](predicate: T => Boolean, description: String): Validator[T, T] =
    Condition(predicate, description)

  def min[T: PartialOrd](min: T, inclusive: Boolean = true): Label ?=> Validator[T, T] = {
    condition(
      predicate = value => if (inclusive) value >= min else value > min,
      description =
        if (inclusive) s"${summon[Label].label} must be more then or equal to $min"
        else s"${summon[Label].label} must be more then $min",
    )
  }

  def max[T: PartialOrd](max: T, inclusive: Boolean = true): Label ?=> Validator[T, T] =
    condition(
      predicate = value => if (inclusive) value <= max else value < max,
      description =
        if (inclusive) s"${summon[Label].label} must be less then or equal to $max"
        else s"${summon[Label].label} must be less then $max",
    )

  def notEmpty: Label ?=> Validator[String, String] =
    condition(
      predicate = _.nonEmpty,
      description = s"${summon[Label].label} must not be empty",
    )

  def notBlank: Label ?=> Validator[String, String] =
    condition(
      predicate = value => !value.isBlank,
      description = s"${summon[Label].label} must not be blank",
    )

  def minLength(minLength: Int, inclusive: Boolean = true): Label ?=> Validator[String, String] =
    condition(
      predicate = value => if (inclusive) value.length >= minLength else value.length > minLength,
      description =
        if (inclusive) s"${summon[Label].label} length must be longer then or equal to $minLength"
        else s"${summon[Label].label} length must be longer then $minLength",
    )

  def maxLength[T: PartialOrd](maxLength: Int, inclusive: Boolean = true): Label ?=> Validator[String, String] =
    condition(
      predicate = value => if (inclusive) value.length <= maxLength else value.length < maxLength,
      description =
        if (inclusive) s"${summon[Label].label} length must be shorter then or equal to $maxLength"
        else s"${summon[Label].label} length must be shorter then $maxLength",
    )

  def regExr(regex: String, description: String): Label ?=> Validator[String, String] =
    condition(
      predicate = regex.r.findFirstMatchIn(_).isDefined,
      description = s"${summon[Label].label} $description",
    )

  def all[In, Out](validators: Validator[In, Out]*): Validator[In, List[Out]] =
    ValidateAll(validators.toList)

  def allDiscard[In](validators: Validator[In, ?]*): Validator[In, Unit] =
    ValidateAll(validators.toList.map(_.unit)).unit

  def valid[In, Out]: Validator[In, Out] ?=> Validator[In, Out] =
    summon[Validator[In, Out]]

  private case class Id[T]() extends Validator[T, T] {
    override def validate(value: T): Validation[String, T] =
      Validation.succeed(value)
  }

  private case class Required[T](label: String) extends Validator[Option[T], T] {
    override def validate(value: Option[T]): Validation[String, T] =
      Validation.fromOptionWith(s"$label is required")(value)
  }

  private case class Condition[T](predicate: T => Boolean, description: String) extends Validator[T, T] {
    override def validate(value: T): Validation[String, T] = {
      Validation.fromPredicateWith(description)(value)(predicate)
    }
  }

  private case class ValidateAll[In, Out, F[+_]: ForEach](private val validators: F[Validator[In, Out]])
      extends Validator[In, F[Out]] {
    override def validate(value: In): Validation[String, F[Out]] =
      Validation.validateAll(validators.map(_.validate(value)))
  }

  private case class ValidateForEach[In, Out, F[+_]: ForEach](private val validator: Validator[In, Out])
      extends Validator[F[In], F[Out]] {
    override def validate(value: F[In]): Validation[String, F[Out]] =
      Validation.validateAll(value.map(validator.validate))
  }

  private case class Tap[In, Out, Out2](
      private val validator1: Validator[In, Out],
      private val validator2: Validator[Out, Out2],
  ) extends Validator[In, Out] {
    override def validate(value: In): Validation[String, Out] = {
      validator1.validate(value).tap(validator2.validate)
    }
  }

  private case class Map[In, Out, Out2](private val validator: Validator[In, Out], private val f: Out => Out2)
      extends Validator[In, Out2] {
    override def validate(value: In): Validation[String, Out2] =
      validator.validate(value).map(f)
  }

  private case class ContraMap[In, In2, Out](private val validator: Validator[In2, Out], private val f: In => In2)
      extends Validator[In, Out] {
    override def validate(value: In): Validation[String, Out] =
      validator.validate(f(value))
  }

  private case class FlatMap[In, Out, Out2](
      private val validator1: Validator[In, Out],
      private val validator2: Validator[Out, Out2],
  ) extends Validator[In, Out2] {
    override def validate(value: In): Validation[String, Out2] = {
      validator1.validate(value).flatMap(validator2.validate)
    }
  }

  private case class Zip[In, Out1, Out2, Out](
      private val validator1: Validator[In, Out1],
      private val validator2: Validator[In, Out2],
      private val f: (Out1, Out2) => Out,
  ) extends Validator[In, Out] {
    override def validate(value: In): Validation[String, Out] = {
      validator1.validate(value).zipWithPar(validator2.validate(value))(f)
    }
  }

}
