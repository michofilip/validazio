package validazio

import validazio.Validator.*
import zio.*
import zio.prelude.{PartialOrd, PartialOrdOps}

def validateZIO[In, Out](value: In): Validator[In, Out] ?=> IO[Chunk[ValidationFailure], Out] =
  valid.validate(value).mapError(Chunk.single).toZIOAssociative

def validateZIOWithDescriptor[In, Err, Out](descriptor: ValidationFailureDescriptor[Err])(
    value: In,
): Validator[In, Out] ?=> IO[Chunk[Err], Out] =
  validateZIO(value).mapError(_.map(descriptor.describe))

def validateZIOWithDefaultDescriptor[In, Out](
    value: In,
): Validator[In, Out] ?=> IO[Chunk[String], Out] =
  validateZIOWithDescriptor(ValidationFailureDescriptor.defaultDescriptor)(value)

def labeled[In, Out](label: String)(validator: Label ?=> Validator[In, Out]): Validator[In, Out] =
  validator(using Label(label))

def id[T]: Validator[T, T] =
  IdValidator()

def required[T]: Label ?=> Validator[Option[T], T] =
  RequiredValidator(summon[Label])

def condition[T](predicate: T => Boolean, description: String): Validator[T, T] =
  ConditionValidator(
    predicate = predicate,
    validationFailure = ValidationFailure.Condition(description),
  )

def isTrue: Label ?=> Validator[Boolean, Boolean] =
  ConditionValidator(
    predicate = identity,
    validationFailure = ValidationFailure.IsTrue(summon[Label]),
  )

def isFalse: Label ?=> Validator[Boolean, Boolean] =
  ConditionValidator(
    predicate = !_,
    validationFailure = ValidationFailure.IsFalse(summon[Label]),
  )

def min[T: PartialOrd](min: T, inclusive: Boolean = true): Label ?=> Validator[T, T] =
  ConditionValidator(
    predicate = value => if (inclusive) value >= min else value > min,
    validationFailure = ValidationFailure.Min(summon[Label], min, inclusive),
  )

def max[T: PartialOrd](max: T, inclusive: Boolean = true): Label ?=> Validator[T, T] =
  ConditionValidator(
    predicate = value => if (inclusive) value <= max else value < max,
    validationFailure = ValidationFailure.Max(summon[Label], max, inclusive),
  )

def minSize[T <: Seq[?]](minSize: Int, inclusive: Boolean = true): Label ?=> Validator[T, T] =
  ConditionValidator(
    predicate = value => if (inclusive) value.size >= minSize else value.size > minSize,
    validationFailure = ValidationFailure.MinSize(summon[Label], minSize, inclusive),
  )

def maxSize[T <: Seq[?]](maxSize: Int, inclusive: Boolean = true): Label ?=> Validator[T, T] =
  ConditionValidator(
    predicate = value => if (inclusive) value.size <= maxSize else value.size < maxSize,
    validationFailure = ValidationFailure.MaxSize(summon[Label], maxSize, inclusive),
  )

def minSetSize[T <: Set[?]](minSize: Int, inclusive: Boolean = true): Label ?=> Validator[T, T] =
  ConditionValidator(
    predicate = value => if (inclusive) value.size >= minSize else value.size > minSize,
    validationFailure = ValidationFailure.MinSetSize(summon[Label], minSize, inclusive),
  )

def maxSetSize[T <: Set[?]](maxSize: Int, inclusive: Boolean = true): Label ?=> Validator[T, T] =
  ConditionValidator(
    predicate = value => if (inclusive) value.size <= maxSize else value.size < maxSize,
    validationFailure = ValidationFailure.MaxSetSize(summon[Label], maxSize, inclusive),
  )

def notEmpty: Label ?=> Validator[String, String] =
  ConditionValidator(
    predicate = _.nonEmpty,
    validationFailure = ValidationFailure.NotEmpty(summon[Label]),
  )

def notBlank: Label ?=> Validator[String, String] =
  ConditionValidator(
    predicate = value => !value.isBlank,
    validationFailure = ValidationFailure.NotBlank(summon[Label]),
  )

def minLength(minLength: Int, inclusive: Boolean = true): Label ?=> Validator[String, String] =
  ConditionValidator(
    predicate = value => if (inclusive) value.length >= minLength else value.length > minLength,
    validationFailure = ValidationFailure.MinLength(summon[Label], minLength, inclusive),
  )

def maxLength[T: PartialOrd](maxLength: Int, inclusive: Boolean = true): Label ?=> Validator[String, String] =
  ConditionValidator(
    predicate = value => if (inclusive) value.length <= maxLength else value.length < maxLength,
    validationFailure = ValidationFailure.MaxLength(summon[Label], maxLength, inclusive),
  )

def regExr(regex: String, description: String): Label ?=> Validator[String, String] =
  ConditionValidator(
    predicate = regex.r.findFirstMatchIn(_).isDefined,
    validationFailure = ValidationFailure.RegExr(summon[Label], regex, description),
  )

def all[In, Out](validators: Validator[In, Out]*): Validator[In, List[Out]] =
  AllValidator(validators.toList)

def allDiscard[In](validators: Validator[In, ?]*): Validator[In, Unit] =
  AllValidator(validators.toList.map(_.unit)).unit

def valid[In, Out]: Validator[In, Out] ?=> Validator[In, Out] =
  summon[Validator[In, Out]]
