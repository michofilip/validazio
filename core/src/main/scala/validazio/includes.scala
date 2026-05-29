package validazio

import validazio.Validator.*
import zio.IO
import zio.prelude.{Associative, PartialOrd, PartialOrdOps}

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

def isTrue: Label ?=> Validator[Boolean, Boolean] =
  condition(
    predicate = _ == true,
    description = s"${summon[Label].label} must be true",
  )

def isFalse: Label ?=> Validator[Boolean, Boolean] =
  condition(
    predicate = _ == false,
    description = s"${summon[Label].label} must be false",
  )

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

def minSize[T <: Seq[?]](minSize: Int, inclusive: Boolean = true): Label ?=> Validator[T, T] =
  condition(
    predicate = value => if (inclusive) value.size >= minSize else value.size > minSize,
    description =
      if (inclusive) s"${summon[Label].label} size must be more then or equal to $minSize"
      else s"${summon[Label].label} size must be more then $minSize",
  )

def maxSize[T <: Seq[?]](maxSize: Int, inclusive: Boolean = true): Label ?=> Validator[T, T] =
  condition(
    predicate = value => if (inclusive) value.size <= maxSize else value.size < maxSize,
    description =
      if (inclusive) s"${summon[Label].label} size must be less then or equal to $maxSize"
      else s"${summon[Label].label} size must be less then $maxSize",
  )

def minSetSize[T <: Set[?]](minSize: Int, inclusive: Boolean = true): Label ?=> Validator[T, T] =
  condition(
    predicate = value => if (inclusive) value.size >= minSize else value.size > minSize,
    description =
      if (inclusive) s"${summon[Label].label} size must be more then or equal to $minSize"
      else s"${summon[Label].label} size must be more then $minSize",
  )

def maxSetSize[T <: Set[?]](maxSize: Int, inclusive: Boolean = true): Label ?=> Validator[T, T] =
  condition(
    predicate = value => if (inclusive) value.size <= maxSize else value.size < maxSize,
    description =
      if (inclusive) s"${summon[Label].label} size must be less then or equal to $maxSize"
      else s"${summon[Label].label} size must be less then $maxSize",
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
