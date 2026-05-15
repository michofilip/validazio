package validazio

import validazio.Validator.*
import zio.*
import zio.prelude.*

trait Validator[In, Out] {
  def validate(value: In): Validation[String, Out]

  final def optional: Validator[Option[In], Option[Out]]                        = ValidateForEach(this)
  final def list: Validator[List[In], List[Out]]                                = ValidateForEach(this)
  final def map[Out2](f: Out => Out2): Validator[In, Out2]                      = Map(this, f)
  final def contraMap[In0](f: In0 => In): Validator[In0, Out]                   = ContraMap(this, f)
  final def flatMap[Out2](validator: Validator[Out, Out2]): Validator[In, Out2] = FlatMap(this, validator)
  final def >>[Out2](validator: Validator[Out, Out2]): Validator[In, Out2]      = flatMap(validator)
  final def tap[Out2](validator: Validator[Out, Out2]): Validator[In, Out]      = Tap(this, validator)
  final def <<[Out2](validator: Validator[Out, Out2]): Validator[In, Out]       = tap(validator)
  final def unit: Validator[In, Unit]                                           = map(_ => ())
}

object Validator {
  def validateZIO[In, Err: Associative, Out](f: String => Err)(value: In)(using Validator[In, Out]): IO[Err, Out] = {
    valid.validate(value).mapError(f).toZIOAssociative
  }

  case class Label(label: String)

  def labeled[In, Out](label: String)(validator: Label ?=> Validator[In, Out]): Validator[In, Out] = {
    validator(using Label(label))
  }

  def id[T]: Validator[T, T] =
    Id()

  def required[T](using Label): Validator[Option[T], T] =
    Required(summon[Label].label)

  def condition[T](predicate: T => Boolean, description: String): Validator[T, T] =
    Condition(predicate, description)

  def min[T: PartialOrd](min: T, inclusive: Boolean = true)(using Label): Validator[T, T] = {
    condition(
      predicate = value => if (inclusive) value >= min else value > min,
      description =
        if (inclusive) s"${summon[Label].label} must be more then or equal to $min"
        else s"${summon[Label].label} must be more then $min",
    )
  }

  def max[T: PartialOrd](max: T, inclusive: Boolean = true)(using Label): Validator[T, T] =
    condition(
      predicate = value => if (inclusive) value <= max else value < max,
      description =
        if (inclusive) s"${summon[Label].label} must be less then or equal to $max"
        else s"${summon[Label].label} must be less then $max",
    )

  def notEmpty(using Label): Validator[String, String] =
    condition(
      predicate = _.nonEmpty,
      description = s"${summon[Label].label} must not be empty",
    )

  def notBlank(using Label): Validator[String, String] =
    condition(
      predicate = value => !value.isBlank,
      description = s"${summon[Label].label} must not be blank",
    )

  def minLength(minLength: Int, inclusive: Boolean = true)(using Label): Validator[String, String] =
    condition(
      predicate = value => if (inclusive) value.length >= minLength else value.length > minLength,
      description =
        if (inclusive) s"${summon[Label].label} length must be longer then or equal to $minLength"
        else s"${summon[Label].label} length must be longer then $minLength",
    )

  def maxLength[T: PartialOrd](maxLength: Int, inclusive: Boolean = true)(using Label): Validator[String, String] =
    condition(
      predicate = value => if (inclusive) value.length <= maxLength else value.length < maxLength,
      description =
        if (inclusive) s"${summon[Label].label} length must be shorter then or equal to $maxLength"
        else s"${summon[Label].label} length must be shorter then $maxLength",
    )

  def regExr(regex: String, description: String)(using Label): Validator[String, String] =
    condition(
      predicate = regex.r.findFirstMatchIn(_).isDefined,
      description = s"${summon[Label].label} $description",
    )

  def all[In, Out](validators: Validator[In, Out]*): Validator[In, List[Out]] =
    ValidateAll(validators.toList)

  def allDiscard[In](validators: Validator[In, ?]*): Validator[In, Unit] =
    ValidateAll(validators.toList.map(_.unit)).unit

  def valid[In, Out](using Validator[In, Out]): Validator[In, Out] =
    summon[Validator[In, Out]]

  def validateWith[In, Out1, Out2, Out](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
  )(f: (Out1, Out2) => Out): Validator[In, Out] =
    ValidateWith2(validator1, validator2)(f)

  def validateWith[In, Out1, Out2, Out3, Out](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
  )(f: (Out1, Out2, Out3) => Out): Validator[In, Out] =
    ValidateWith3(validator1, validator2, validator3)(f)

  def validateWith[In, Out1, Out2, Out3, Out4, Out](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
  )(f: (Out1, Out2, Out3, Out4) => Out): Validator[In, Out] =
    ValidateWith4(validator1, validator2, validator3, validator4)(f)

  def validateWith[In, Out1, Out2, Out3, Out4, Out5, Out](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
      validator5: Validator[In, Out5],
  )(f: (Out1, Out2, Out3, Out4, Out5) => Out): Validator[In, Out] =
    ValidateWith5(validator1, validator2, validator3, validator4, validator5)(f)

  def validateWith[In, Out1, Out2, Out3, Out4, Out5, Out6, Out](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
      validator5: Validator[In, Out5],
      validator6: Validator[In, Out6],
  )(f: (Out1, Out2, Out3, Out4, Out5, Out6) => Out): Validator[In, Out] =
    ValidateWith6(validator1, validator2, validator3, validator4, validator5, validator6)(f)

  def validateWith[In, Out1, Out2, Out3, Out4, Out5, Out6, Out7, Out](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
      validator5: Validator[In, Out5],
      validator6: Validator[In, Out6],
      validator7: Validator[In, Out7],
  )(f: (Out1, Out2, Out3, Out4, Out5, Out6, Out7) => Out): Validator[In, Out] =
    ValidateWith7(validator1, validator2, validator3, validator4, validator5, validator6, validator7)(f)

  def validateWith[In, Out1, Out2, Out3, Out4, Out5, Out6, Out7, Out8, Out](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
      validator5: Validator[In, Out5],
      validator6: Validator[In, Out6],
      validator7: Validator[In, Out7],
      validator8: Validator[In, Out8],
  )(f: (Out1, Out2, Out3, Out4, Out5, Out6, Out7, Out8) => Out): Validator[In, Out] =
    ValidateWith8(validator1, validator2, validator3, validator4, validator5, validator6, validator7, validator8)(f)

  def validateWith[In, Out1, Out2, Out3, Out4, Out5, Out6, Out7, Out8, Out9, Out](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
      validator5: Validator[In, Out5],
      validator6: Validator[In, Out6],
      validator7: Validator[In, Out7],
      validator8: Validator[In, Out8],
      validator9: Validator[In, Out9],
  )(f: (Out1, Out2, Out3, Out4, Out5, Out6, Out7, Out8, Out9) => Out): Validator[In, Out] =
    ValidateWith9(
      validator1,
      validator2,
      validator3,
      validator4,
      validator5,
      validator6,
      validator7,
      validator8,
      validator9,
    )(f)

  def validateWith[In, Out1, Out2, Out3, Out4, Out5, Out6, Out7, Out8, Out9, Out10, Out](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
      validator5: Validator[In, Out5],
      validator6: Validator[In, Out6],
      validator7: Validator[In, Out7],
      validator8: Validator[In, Out8],
      validator9: Validator[In, Out9],
      validator10: Validator[In, Out10],
  )(f: (Out1, Out2, Out3, Out4, Out5, Out6, Out7, Out8, Out9, Out10) => Out): Validator[In, Out] =
    ValidateWith10(
      validator1,
      validator2,
      validator3,
      validator4,
      validator5,
      validator6,
      validator7,
      validator8,
      validator9,
      validator10,
    )(f)

  def validateWith[In, Out1, Out2, Out3, Out4, Out5, Out6, Out7, Out8, Out9, Out10, Out11, Out](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
      validator5: Validator[In, Out5],
      validator6: Validator[In, Out6],
      validator7: Validator[In, Out7],
      validator8: Validator[In, Out8],
      validator9: Validator[In, Out9],
      validator10: Validator[In, Out10],
      validator11: Validator[In, Out11],
  )(f: (Out1, Out2, Out3, Out4, Out5, Out6, Out7, Out8, Out9, Out10, Out11) => Out): Validator[In, Out] =
    ValidateWith11(
      validator1,
      validator2,
      validator3,
      validator4,
      validator5,
      validator6,
      validator7,
      validator8,
      validator9,
      validator10,
      validator11,
    )(f)

  def validateWith[
      In,
      Out1,
      Out2,
      Out3,
      Out4,
      Out5,
      Out6,
      Out7,
      Out8,
      Out9,
      Out10,
      Out11,
      Out12,
      Out,
  ](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
      validator5: Validator[In, Out5],
      validator6: Validator[In, Out6],
      validator7: Validator[In, Out7],
      validator8: Validator[In, Out8],
      validator9: Validator[In, Out9],
      validator10: Validator[In, Out10],
      validator11: Validator[In, Out11],
      validator12: Validator[In, Out12],
  )(f: (Out1, Out2, Out3, Out4, Out5, Out6, Out7, Out8, Out9, Out10, Out11, Out12) => Out): Validator[In, Out] =
    ValidateWith12(
      validator1,
      validator2,
      validator3,
      validator4,
      validator5,
      validator6,
      validator7,
      validator8,
      validator9,
      validator10,
      validator11,
      validator12,
    )(f)

  def validateWith[
      In,
      Out1,
      Out2,
      Out3,
      Out4,
      Out5,
      Out6,
      Out7,
      Out8,
      Out9,
      Out10,
      Out11,
      Out12,
      Out13,
      Out,
  ](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
      validator5: Validator[In, Out5],
      validator6: Validator[In, Out6],
      validator7: Validator[In, Out7],
      validator8: Validator[In, Out8],
      validator9: Validator[In, Out9],
      validator10: Validator[In, Out10],
      validator11: Validator[In, Out11],
      validator12: Validator[In, Out12],
      validator13: Validator[In, Out13],
  )(f: (Out1, Out2, Out3, Out4, Out5, Out6, Out7, Out8, Out9, Out10, Out11, Out12, Out13) => Out): Validator[In, Out] =
    ValidateWith13(
      validator1,
      validator2,
      validator3,
      validator4,
      validator5,
      validator6,
      validator7,
      validator8,
      validator9,
      validator10,
      validator11,
      validator12,
      validator13,
    )(f)

  def validateWith[
      In,
      Out1,
      Out2,
      Out3,
      Out4,
      Out5,
      Out6,
      Out7,
      Out8,
      Out9,
      Out10,
      Out11,
      Out12,
      Out13,
      Out14,
      Out,
  ](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
      validator5: Validator[In, Out5],
      validator6: Validator[In, Out6],
      validator7: Validator[In, Out7],
      validator8: Validator[In, Out8],
      validator9: Validator[In, Out9],
      validator10: Validator[In, Out10],
      validator11: Validator[In, Out11],
      validator12: Validator[In, Out12],
      validator13: Validator[In, Out13],
      validator14: Validator[In, Out14],
  )(
      f: (Out1, Out2, Out3, Out4, Out5, Out6, Out7, Out8, Out9, Out10, Out11, Out12, Out13, Out14) => Out,
  ): Validator[In, Out] =
    ValidateWith14(
      validator1,
      validator2,
      validator3,
      validator4,
      validator5,
      validator6,
      validator7,
      validator8,
      validator9,
      validator10,
      validator11,
      validator12,
      validator13,
      validator14,
    )(f)

  def validateWith[
      In,
      Out1,
      Out2,
      Out3,
      Out4,
      Out5,
      Out6,
      Out7,
      Out8,
      Out9,
      Out10,
      Out11,
      Out12,
      Out13,
      Out14,
      Out15,
      Out,
  ](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
      validator5: Validator[In, Out5],
      validator6: Validator[In, Out6],
      validator7: Validator[In, Out7],
      validator8: Validator[In, Out8],
      validator9: Validator[In, Out9],
      validator10: Validator[In, Out10],
      validator11: Validator[In, Out11],
      validator12: Validator[In, Out12],
      validator13: Validator[In, Out13],
      validator14: Validator[In, Out14],
      validator15: Validator[In, Out15],
  )(
      f: (Out1, Out2, Out3, Out4, Out5, Out6, Out7, Out8, Out9, Out10, Out11, Out12, Out13, Out14, Out15) => Out,
  ): Validator[In, Out] =
    ValidateWith15(
      validator1,
      validator2,
      validator3,
      validator4,
      validator5,
      validator6,
      validator7,
      validator8,
      validator9,
      validator10,
      validator11,
      validator12,
      validator13,
      validator14,
      validator15,
    )(f)

  def validateWith[
      In,
      Out1,
      Out2,
      Out3,
      Out4,
      Out5,
      Out6,
      Out7,
      Out8,
      Out9,
      Out10,
      Out11,
      Out12,
      Out13,
      Out14,
      Out15,
      Out16,
      Out,
  ](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
      validator5: Validator[In, Out5],
      validator6: Validator[In, Out6],
      validator7: Validator[In, Out7],
      validator8: Validator[In, Out8],
      validator9: Validator[In, Out9],
      validator10: Validator[In, Out10],
      validator11: Validator[In, Out11],
      validator12: Validator[In, Out12],
      validator13: Validator[In, Out13],
      validator14: Validator[In, Out14],
      validator15: Validator[In, Out15],
      validator16: Validator[In, Out16],
  )(
      f: (
          Out1,
          Out2,
          Out3,
          Out4,
          Out5,
          Out6,
          Out7,
          Out8,
          Out9,
          Out10,
          Out11,
          Out12,
          Out13,
          Out14,
          Out15,
          Out16,
      ) => Out,
  ): Validator[In, Out] =
    ValidateWith16(
      validator1,
      validator2,
      validator3,
      validator4,
      validator5,
      validator6,
      validator7,
      validator8,
      validator9,
      validator10,
      validator11,
      validator12,
      validator13,
      validator14,
      validator15,
      validator16,
    )(f)

  def validateWith[
      In,
      Out1,
      Out2,
      Out3,
      Out4,
      Out5,
      Out6,
      Out7,
      Out8,
      Out9,
      Out10,
      Out11,
      Out12,
      Out13,
      Out14,
      Out15,
      Out16,
      Out17,
      Out,
  ](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
      validator5: Validator[In, Out5],
      validator6: Validator[In, Out6],
      validator7: Validator[In, Out7],
      validator8: Validator[In, Out8],
      validator9: Validator[In, Out9],
      validator10: Validator[In, Out10],
      validator11: Validator[In, Out11],
      validator12: Validator[In, Out12],
      validator13: Validator[In, Out13],
      validator14: Validator[In, Out14],
      validator15: Validator[In, Out15],
      validator16: Validator[In, Out16],
      validator17: Validator[In, Out17],
  )(
      f: (
          Out1,
          Out2,
          Out3,
          Out4,
          Out5,
          Out6,
          Out7,
          Out8,
          Out9,
          Out10,
          Out11,
          Out12,
          Out13,
          Out14,
          Out15,
          Out16,
          Out17,
      ) => Out,
  ): Validator[In, Out] =
    ValidateWith17(
      validator1,
      validator2,
      validator3,
      validator4,
      validator5,
      validator6,
      validator7,
      validator8,
      validator9,
      validator10,
      validator11,
      validator12,
      validator13,
      validator14,
      validator15,
      validator16,
      validator17,
    )(f)

  def validateWith[
      In,
      Out1,
      Out2,
      Out3,
      Out4,
      Out5,
      Out6,
      Out7,
      Out8,
      Out9,
      Out10,
      Out11,
      Out12,
      Out13,
      Out14,
      Out15,
      Out16,
      Out17,
      Out18,
      Out,
  ](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
      validator5: Validator[In, Out5],
      validator6: Validator[In, Out6],
      validator7: Validator[In, Out7],
      validator8: Validator[In, Out8],
      validator9: Validator[In, Out9],
      validator10: Validator[In, Out10],
      validator11: Validator[In, Out11],
      validator12: Validator[In, Out12],
      validator13: Validator[In, Out13],
      validator14: Validator[In, Out14],
      validator15: Validator[In, Out15],
      validator16: Validator[In, Out16],
      validator17: Validator[In, Out17],
      validator18: Validator[In, Out18],
  )(
      f: (
          Out1,
          Out2,
          Out3,
          Out4,
          Out5,
          Out6,
          Out7,
          Out8,
          Out9,
          Out10,
          Out11,
          Out12,
          Out13,
          Out14,
          Out15,
          Out16,
          Out17,
          Out18,
      ) => Out,
  ): Validator[In, Out] =
    ValidateWith18(
      validator1,
      validator2,
      validator3,
      validator4,
      validator5,
      validator6,
      validator7,
      validator8,
      validator9,
      validator10,
      validator11,
      validator12,
      validator13,
      validator14,
      validator15,
      validator16,
      validator17,
      validator18,
    )(f)

  def validateWith[
      In,
      Out1,
      Out2,
      Out3,
      Out4,
      Out5,
      Out6,
      Out7,
      Out8,
      Out9,
      Out10,
      Out11,
      Out12,
      Out13,
      Out14,
      Out15,
      Out16,
      Out17,
      Out18,
      Out19,
      Out,
  ](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
      validator5: Validator[In, Out5],
      validator6: Validator[In, Out6],
      validator7: Validator[In, Out7],
      validator8: Validator[In, Out8],
      validator9: Validator[In, Out9],
      validator10: Validator[In, Out10],
      validator11: Validator[In, Out11],
      validator12: Validator[In, Out12],
      validator13: Validator[In, Out13],
      validator14: Validator[In, Out14],
      validator15: Validator[In, Out15],
      validator16: Validator[In, Out16],
      validator17: Validator[In, Out17],
      validator18: Validator[In, Out18],
      validator19: Validator[In, Out19],
  )(
      f: (
          Out1,
          Out2,
          Out3,
          Out4,
          Out5,
          Out6,
          Out7,
          Out8,
          Out9,
          Out10,
          Out11,
          Out12,
          Out13,
          Out14,
          Out15,
          Out16,
          Out17,
          Out18,
          Out19,
      ) => Out,
  ): Validator[In, Out] =
    ValidateWith19(
      validator1,
      validator2,
      validator3,
      validator4,
      validator5,
      validator6,
      validator7,
      validator8,
      validator9,
      validator10,
      validator11,
      validator12,
      validator13,
      validator14,
      validator15,
      validator16,
      validator17,
      validator18,
      validator19,
    )(f)

  def validateWith[
      In,
      Out1,
      Out2,
      Out3,
      Out4,
      Out5,
      Out6,
      Out7,
      Out8,
      Out9,
      Out10,
      Out11,
      Out12,
      Out13,
      Out14,
      Out15,
      Out16,
      Out17,
      Out18,
      Out19,
      Out20,
      Out,
  ](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
      validator5: Validator[In, Out5],
      validator6: Validator[In, Out6],
      validator7: Validator[In, Out7],
      validator8: Validator[In, Out8],
      validator9: Validator[In, Out9],
      validator10: Validator[In, Out10],
      validator11: Validator[In, Out11],
      validator12: Validator[In, Out12],
      validator13: Validator[In, Out13],
      validator14: Validator[In, Out14],
      validator15: Validator[In, Out15],
      validator16: Validator[In, Out16],
      validator17: Validator[In, Out17],
      validator18: Validator[In, Out18],
      validator19: Validator[In, Out19],
      validator20: Validator[In, Out20],
  )(
      f: (
          Out1,
          Out2,
          Out3,
          Out4,
          Out5,
          Out6,
          Out7,
          Out8,
          Out9,
          Out10,
          Out11,
          Out12,
          Out13,
          Out14,
          Out15,
          Out16,
          Out17,
          Out18,
          Out19,
          Out20,
      ) => Out,
  ): Validator[In, Out] =
    ValidateWith20(
      validator1,
      validator2,
      validator3,
      validator4,
      validator5,
      validator6,
      validator7,
      validator8,
      validator9,
      validator10,
      validator11,
      validator12,
      validator13,
      validator14,
      validator15,
      validator16,
      validator17,
      validator18,
      validator19,
      validator20,
    )(f)

  def validateWith[
      In,
      Out1,
      Out2,
      Out3,
      Out4,
      Out5,
      Out6,
      Out7,
      Out8,
      Out9,
      Out10,
      Out11,
      Out12,
      Out13,
      Out14,
      Out15,
      Out16,
      Out17,
      Out18,
      Out19,
      Out20,
      Out21,
      Out,
  ](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
      validator5: Validator[In, Out5],
      validator6: Validator[In, Out6],
      validator7: Validator[In, Out7],
      validator8: Validator[In, Out8],
      validator9: Validator[In, Out9],
      validator10: Validator[In, Out10],
      validator11: Validator[In, Out11],
      validator12: Validator[In, Out12],
      validator13: Validator[In, Out13],
      validator14: Validator[In, Out14],
      validator15: Validator[In, Out15],
      validator16: Validator[In, Out16],
      validator17: Validator[In, Out17],
      validator18: Validator[In, Out18],
      validator19: Validator[In, Out19],
      validator20: Validator[In, Out20],
      validator21: Validator[In, Out21],
  )(
      f: (
          Out1,
          Out2,
          Out3,
          Out4,
          Out5,
          Out6,
          Out7,
          Out8,
          Out9,
          Out10,
          Out11,
          Out12,
          Out13,
          Out14,
          Out15,
          Out16,
          Out17,
          Out18,
          Out19,
          Out20,
          Out21,
      ) => Out,
  ): Validator[In, Out] =
    ValidateWith21(
      validator1,
      validator2,
      validator3,
      validator4,
      validator5,
      validator6,
      validator7,
      validator8,
      validator9,
      validator10,
      validator11,
      validator12,
      validator13,
      validator14,
      validator15,
      validator16,
      validator17,
      validator18,
      validator19,
      validator20,
      validator21,
    )(f)

  def validateWith[
      In,
      Out1,
      Out2,
      Out3,
      Out4,
      Out5,
      Out6,
      Out7,
      Out8,
      Out9,
      Out10,
      Out11,
      Out12,
      Out13,
      Out14,
      Out15,
      Out16,
      Out17,
      Out18,
      Out19,
      Out20,
      Out21,
      Out22,
      Out,
  ](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
      validator5: Validator[In, Out5],
      validator6: Validator[In, Out6],
      validator7: Validator[In, Out7],
      validator8: Validator[In, Out8],
      validator9: Validator[In, Out9],
      validator10: Validator[In, Out10],
      validator11: Validator[In, Out11],
      validator12: Validator[In, Out12],
      validator13: Validator[In, Out13],
      validator14: Validator[In, Out14],
      validator15: Validator[In, Out15],
      validator16: Validator[In, Out16],
      validator17: Validator[In, Out17],
      validator18: Validator[In, Out18],
      validator19: Validator[In, Out19],
      validator20: Validator[In, Out20],
      validator21: Validator[In, Out21],
      validator22: Validator[In, Out22],
  )(
      f: (
          Out1,
          Out2,
          Out3,
          Out4,
          Out5,
          Out6,
          Out7,
          Out8,
          Out9,
          Out10,
          Out11,
          Out12,
          Out13,
          Out14,
          Out15,
          Out16,
          Out17,
          Out18,
          Out19,
          Out20,
          Out21,
          Out22,
      ) => Out,
  ): Validator[In, Out] =
    ValidateWith22(
      validator1,
      validator2,
      validator3,
      validator4,
      validator5,
      validator6,
      validator7,
      validator8,
      validator9,
      validator10,
      validator11,
      validator12,
      validator13,
      validator14,
      validator15,
      validator16,
      validator17,
      validator18,
      validator19,
      validator20,
      validator21,
      validator22,
    )(f)

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

  private case class ValidateWith2[In, Out1, Out2, Out](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
  )(f: (Out1, Out2) => Out)
      extends Validator[In, Out] {
    override def validate(value: In): Validation[String, Out] =
      Validation.validateWith(
        validator1.validate(value),
        validator2.validate(value),
      )(f)
  }

  private case class ValidateWith3[In, Out1, Out2, Out3, Out](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
  )(f: (Out1, Out2, Out3) => Out)
      extends Validator[In, Out] {
    override def validate(value: In): Validation[String, Out] =
      Validation.validateWith(
        validator1.validate(value),
        validator2.validate(value),
        validator3.validate(value),
      )(f)
  }

  private case class ValidateWith4[In, Out1, Out2, Out3, Out4, Out](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
  )(f: (Out1, Out2, Out3, Out4) => Out)
      extends Validator[In, Out] {
    override def validate(value: In): Validation[String, Out] =
      Validation.validateWith(
        validator1.validate(value),
        validator2.validate(value),
        validator3.validate(value),
        validator4.validate(value),
      )(f)
  }

  private case class ValidateWith5[In, Out1, Out2, Out3, Out4, Out5, Out](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
      validator5: Validator[In, Out5],
  )(f: (Out1, Out2, Out3, Out4, Out5) => Out)
      extends Validator[In, Out] {
    override def validate(value: In): Validation[String, Out] =
      Validation.validateWith(
        validator1.validate(value),
        validator2.validate(value),
        validator3.validate(value),
        validator4.validate(value),
        validator5.validate(value),
      )(f)
  }

  private case class ValidateWith6[In, Out1, Out2, Out3, Out4, Out5, Out6, Out](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
      validator5: Validator[In, Out5],
      validator6: Validator[In, Out6],
  )(f: (Out1, Out2, Out3, Out4, Out5, Out6) => Out)
      extends Validator[In, Out] {
    override def validate(value: In): Validation[String, Out] =
      Validation.validateWith(
        validator1.validate(value),
        validator2.validate(value),
        validator3.validate(value),
        validator4.validate(value),
        validator5.validate(value),
        validator6.validate(value),
      )(f)
  }

  private case class ValidateWith7[In, Out1, Out2, Out3, Out4, Out5, Out6, Out7, Out](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
      validator5: Validator[In, Out5],
      validator6: Validator[In, Out6],
      validator7: Validator[In, Out7],
  )(f: (Out1, Out2, Out3, Out4, Out5, Out6, Out7) => Out)
      extends Validator[In, Out] {
    override def validate(value: In): Validation[String, Out] =
      Validation.validateWith(
        validator1.validate(value),
        validator2.validate(value),
        validator3.validate(value),
        validator4.validate(value),
        validator5.validate(value),
        validator6.validate(value),
        validator7.validate(value),
      )(f)
  }

  private case class ValidateWith8[In, Out1, Out2, Out3, Out4, Out5, Out6, Out7, Out8, Out](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
      validator5: Validator[In, Out5],
      validator6: Validator[In, Out6],
      validator7: Validator[In, Out7],
      validator8: Validator[In, Out8],
  )(f: (Out1, Out2, Out3, Out4, Out5, Out6, Out7, Out8) => Out)
      extends Validator[In, Out] {
    override def validate(value: In): Validation[String, Out] =
      Validation.validateWith(
        validator1.validate(value),
        validator2.validate(value),
        validator3.validate(value),
        validator4.validate(value),
        validator5.validate(value),
        validator6.validate(value),
        validator7.validate(value),
        validator8.validate(value),
      )(f)
  }

  private case class ValidateWith9[In, Out1, Out2, Out3, Out4, Out5, Out6, Out7, Out8, Out9, Out](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
      validator5: Validator[In, Out5],
      validator6: Validator[In, Out6],
      validator7: Validator[In, Out7],
      validator8: Validator[In, Out8],
      validator9: Validator[In, Out9],
  )(f: (Out1, Out2, Out3, Out4, Out5, Out6, Out7, Out8, Out9) => Out)
      extends Validator[In, Out] {
    override def validate(value: In): Validation[String, Out] =
      Validation.validateWith(
        validator1.validate(value),
        validator2.validate(value),
        validator3.validate(value),
        validator4.validate(value),
        validator5.validate(value),
        validator6.validate(value),
        validator7.validate(value),
        validator8.validate(value),
        validator9.validate(value),
      )(f)
  }

  private case class ValidateWith10[In, Out1, Out2, Out3, Out4, Out5, Out6, Out7, Out8, Out9, Out10, Out](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
      validator5: Validator[In, Out5],
      validator6: Validator[In, Out6],
      validator7: Validator[In, Out7],
      validator8: Validator[In, Out8],
      validator9: Validator[In, Out9],
      validator10: Validator[In, Out10],
  )(f: (Out1, Out2, Out3, Out4, Out5, Out6, Out7, Out8, Out9, Out10) => Out)
      extends Validator[In, Out] {
    override def validate(value: In): Validation[String, Out] =
      Validation.validateWith(
        validator1.validate(value),
        validator2.validate(value),
        validator3.validate(value),
        validator4.validate(value),
        validator5.validate(value),
        validator6.validate(value),
        validator7.validate(value),
        validator8.validate(value),
        validator9.validate(value),
        validator10.validate(value),
      )(f)
  }

  private case class ValidateWith11[In, Out1, Out2, Out3, Out4, Out5, Out6, Out7, Out8, Out9, Out10, Out11, Out](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
      validator5: Validator[In, Out5],
      validator6: Validator[In, Out6],
      validator7: Validator[In, Out7],
      validator8: Validator[In, Out8],
      validator9: Validator[In, Out9],
      validator10: Validator[In, Out10],
      validator11: Validator[In, Out11],
  )(f: (Out1, Out2, Out3, Out4, Out5, Out6, Out7, Out8, Out9, Out10, Out11) => Out)
      extends Validator[In, Out] {
    override def validate(value: In): Validation[String, Out] =
      Validation.validateWith(
        validator1.validate(value),
        validator2.validate(value),
        validator3.validate(value),
        validator4.validate(value),
        validator5.validate(value),
        validator6.validate(value),
        validator7.validate(value),
        validator8.validate(value),
        validator9.validate(value),
        validator10.validate(value),
        validator11.validate(value),
      )(f)
  }

  private case class ValidateWith12[
      In,
      Out1,
      Out2,
      Out3,
      Out4,
      Out5,
      Out6,
      Out7,
      Out8,
      Out9,
      Out10,
      Out11,
      Out12,
      Out,
  ](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
      validator5: Validator[In, Out5],
      validator6: Validator[In, Out6],
      validator7: Validator[In, Out7],
      validator8: Validator[In, Out8],
      validator9: Validator[In, Out9],
      validator10: Validator[In, Out10],
      validator11: Validator[In, Out11],
      validator12: Validator[In, Out12],
  )(f: (Out1, Out2, Out3, Out4, Out5, Out6, Out7, Out8, Out9, Out10, Out11, Out12) => Out)
      extends Validator[In, Out] {
    override def validate(value: In): Validation[String, Out] =
      Validation.validateWith(
        validator1.validate(value),
        validator2.validate(value),
        validator3.validate(value),
        validator4.validate(value),
        validator5.validate(value),
        validator6.validate(value),
        validator7.validate(value),
        validator8.validate(value),
        validator9.validate(value),
        validator10.validate(value),
        validator11.validate(value),
        validator12.validate(value),
      )(f)
  }

  private case class ValidateWith13[
      In,
      Out1,
      Out2,
      Out3,
      Out4,
      Out5,
      Out6,
      Out7,
      Out8,
      Out9,
      Out10,
      Out11,
      Out12,
      Out13,
      Out,
  ](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
      validator5: Validator[In, Out5],
      validator6: Validator[In, Out6],
      validator7: Validator[In, Out7],
      validator8: Validator[In, Out8],
      validator9: Validator[In, Out9],
      validator10: Validator[In, Out10],
      validator11: Validator[In, Out11],
      validator12: Validator[In, Out12],
      validator13: Validator[In, Out13],
  )(f: (Out1, Out2, Out3, Out4, Out5, Out6, Out7, Out8, Out9, Out10, Out11, Out12, Out13) => Out)
      extends Validator[In, Out] {
    override def validate(value: In): Validation[String, Out] =
      Validation.validateWith(
        validator1.validate(value),
        validator2.validate(value),
        validator3.validate(value),
        validator4.validate(value),
        validator5.validate(value),
        validator6.validate(value),
        validator7.validate(value),
        validator8.validate(value),
        validator9.validate(value),
        validator10.validate(value),
        validator11.validate(value),
        validator12.validate(value),
        validator13.validate(value),
      )(f)
  }

  private case class ValidateWith14[
      In,
      Out1,
      Out2,
      Out3,
      Out4,
      Out5,
      Out6,
      Out7,
      Out8,
      Out9,
      Out10,
      Out11,
      Out12,
      Out13,
      Out14,
      Out,
  ](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
      validator5: Validator[In, Out5],
      validator6: Validator[In, Out6],
      validator7: Validator[In, Out7],
      validator8: Validator[In, Out8],
      validator9: Validator[In, Out9],
      validator10: Validator[In, Out10],
      validator11: Validator[In, Out11],
      validator12: Validator[In, Out12],
      validator13: Validator[In, Out13],
      validator14: Validator[In, Out14],
  )(f: (Out1, Out2, Out3, Out4, Out5, Out6, Out7, Out8, Out9, Out10, Out11, Out12, Out13, Out14) => Out)
      extends Validator[In, Out] {
    override def validate(value: In): Validation[String, Out] =
      Validation.validateWith(
        validator1.validate(value),
        validator2.validate(value),
        validator3.validate(value),
        validator4.validate(value),
        validator5.validate(value),
        validator6.validate(value),
        validator7.validate(value),
        validator8.validate(value),
        validator9.validate(value),
        validator10.validate(value),
        validator11.validate(value),
        validator12.validate(value),
        validator13.validate(value),
        validator14.validate(value),
      )(f)
  }

  private case class ValidateWith15[
      In,
      Out1,
      Out2,
      Out3,
      Out4,
      Out5,
      Out6,
      Out7,
      Out8,
      Out9,
      Out10,
      Out11,
      Out12,
      Out13,
      Out14,
      Out15,
      Out,
  ](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
      validator5: Validator[In, Out5],
      validator6: Validator[In, Out6],
      validator7: Validator[In, Out7],
      validator8: Validator[In, Out8],
      validator9: Validator[In, Out9],
      validator10: Validator[In, Out10],
      validator11: Validator[In, Out11],
      validator12: Validator[In, Out12],
      validator13: Validator[In, Out13],
      validator14: Validator[In, Out14],
      validator15: Validator[In, Out15],
  )(f: (Out1, Out2, Out3, Out4, Out5, Out6, Out7, Out8, Out9, Out10, Out11, Out12, Out13, Out14, Out15) => Out)
      extends Validator[In, Out] {
    override def validate(value: In): Validation[String, Out] =
      Validation.validateWith(
        validator1.validate(value),
        validator2.validate(value),
        validator3.validate(value),
        validator4.validate(value),
        validator5.validate(value),
        validator6.validate(value),
        validator7.validate(value),
        validator8.validate(value),
        validator9.validate(value),
        validator10.validate(value),
        validator11.validate(value),
        validator12.validate(value),
        validator13.validate(value),
        validator14.validate(value),
        validator15.validate(value),
      )(f)
  }

  private case class ValidateWith16[
      In,
      Out1,
      Out2,
      Out3,
      Out4,
      Out5,
      Out6,
      Out7,
      Out8,
      Out9,
      Out10,
      Out11,
      Out12,
      Out13,
      Out14,
      Out15,
      Out16,
      Out,
  ](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
      validator5: Validator[In, Out5],
      validator6: Validator[In, Out6],
      validator7: Validator[In, Out7],
      validator8: Validator[In, Out8],
      validator9: Validator[In, Out9],
      validator10: Validator[In, Out10],
      validator11: Validator[In, Out11],
      validator12: Validator[In, Out12],
      validator13: Validator[In, Out13],
      validator14: Validator[In, Out14],
      validator15: Validator[In, Out15],
      validator16: Validator[In, Out16],
  )(
      f: (
          Out1,
          Out2,
          Out3,
          Out4,
          Out5,
          Out6,
          Out7,
          Out8,
          Out9,
          Out10,
          Out11,
          Out12,
          Out13,
          Out14,
          Out15,
          Out16,
      ) => Out,
  ) extends Validator[In, Out] {
    override def validate(value: In): Validation[String, Out] =
      Validation.validateWith(
        validator1.validate(value),
        validator2.validate(value),
        validator3.validate(value),
        validator4.validate(value),
        validator5.validate(value),
        validator6.validate(value),
        validator7.validate(value),
        validator8.validate(value),
        validator9.validate(value),
        validator10.validate(value),
        validator11.validate(value),
        validator12.validate(value),
        validator13.validate(value),
        validator14.validate(value),
        validator15.validate(value),
        validator16.validate(value),
      )(f)
  }

  private case class ValidateWith17[
      In,
      Out1,
      Out2,
      Out3,
      Out4,
      Out5,
      Out6,
      Out7,
      Out8,
      Out9,
      Out10,
      Out11,
      Out12,
      Out13,
      Out14,
      Out15,
      Out16,
      Out17,
      Out,
  ](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
      validator5: Validator[In, Out5],
      validator6: Validator[In, Out6],
      validator7: Validator[In, Out7],
      validator8: Validator[In, Out8],
      validator9: Validator[In, Out9],
      validator10: Validator[In, Out10],
      validator11: Validator[In, Out11],
      validator12: Validator[In, Out12],
      validator13: Validator[In, Out13],
      validator14: Validator[In, Out14],
      validator15: Validator[In, Out15],
      validator16: Validator[In, Out16],
      validator17: Validator[In, Out17],
  )(
      f: (
          Out1,
          Out2,
          Out3,
          Out4,
          Out5,
          Out6,
          Out7,
          Out8,
          Out9,
          Out10,
          Out11,
          Out12,
          Out13,
          Out14,
          Out15,
          Out16,
          Out17,
      ) => Out,
  ) extends Validator[In, Out] {
    override def validate(value: In): Validation[String, Out] =
      Validation.validateWith(
        validator1.validate(value),
        validator2.validate(value),
        validator3.validate(value),
        validator4.validate(value),
        validator5.validate(value),
        validator6.validate(value),
        validator7.validate(value),
        validator8.validate(value),
        validator9.validate(value),
        validator10.validate(value),
        validator11.validate(value),
        validator12.validate(value),
        validator13.validate(value),
        validator14.validate(value),
        validator15.validate(value),
        validator16.validate(value),
        validator17.validate(value),
      )(f)
  }

  private case class ValidateWith18[
      In,
      Out1,
      Out2,
      Out3,
      Out4,
      Out5,
      Out6,
      Out7,
      Out8,
      Out9,
      Out10,
      Out11,
      Out12,
      Out13,
      Out14,
      Out15,
      Out16,
      Out17,
      Out18,
      Out,
  ](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
      validator5: Validator[In, Out5],
      validator6: Validator[In, Out6],
      validator7: Validator[In, Out7],
      validator8: Validator[In, Out8],
      validator9: Validator[In, Out9],
      validator10: Validator[In, Out10],
      validator11: Validator[In, Out11],
      validator12: Validator[In, Out12],
      validator13: Validator[In, Out13],
      validator14: Validator[In, Out14],
      validator15: Validator[In, Out15],
      validator16: Validator[In, Out16],
      validator17: Validator[In, Out17],
      validator18: Validator[In, Out18],
  )(
      f: (
          Out1,
          Out2,
          Out3,
          Out4,
          Out5,
          Out6,
          Out7,
          Out8,
          Out9,
          Out10,
          Out11,
          Out12,
          Out13,
          Out14,
          Out15,
          Out16,
          Out17,
          Out18,
      ) => Out,
  ) extends Validator[In, Out] {
    override def validate(value: In): Validation[String, Out] =
      Validation.validateWith(
        validator1.validate(value),
        validator2.validate(value),
        validator3.validate(value),
        validator4.validate(value),
        validator5.validate(value),
        validator6.validate(value),
        validator7.validate(value),
        validator8.validate(value),
        validator9.validate(value),
        validator10.validate(value),
        validator11.validate(value),
        validator12.validate(value),
        validator13.validate(value),
        validator14.validate(value),
        validator15.validate(value),
        validator16.validate(value),
        validator17.validate(value),
        validator18.validate(value),
      )(f)
  }

  private case class ValidateWith19[
      In,
      Out1,
      Out2,
      Out3,
      Out4,
      Out5,
      Out6,
      Out7,
      Out8,
      Out9,
      Out10,
      Out11,
      Out12,
      Out13,
      Out14,
      Out15,
      Out16,
      Out17,
      Out18,
      Out19,
      Out,
  ](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
      validator5: Validator[In, Out5],
      validator6: Validator[In, Out6],
      validator7: Validator[In, Out7],
      validator8: Validator[In, Out8],
      validator9: Validator[In, Out9],
      validator10: Validator[In, Out10],
      validator11: Validator[In, Out11],
      validator12: Validator[In, Out12],
      validator13: Validator[In, Out13],
      validator14: Validator[In, Out14],
      validator15: Validator[In, Out15],
      validator16: Validator[In, Out16],
      validator17: Validator[In, Out17],
      validator18: Validator[In, Out18],
      validator19: Validator[In, Out19],
  )(
      f: (
          Out1,
          Out2,
          Out3,
          Out4,
          Out5,
          Out6,
          Out7,
          Out8,
          Out9,
          Out10,
          Out11,
          Out12,
          Out13,
          Out14,
          Out15,
          Out16,
          Out17,
          Out18,
          Out19,
      ) => Out,
  ) extends Validator[In, Out] {
    override def validate(value: In): Validation[String, Out] =
      Validation.validateWith(
        validator1.validate(value),
        validator2.validate(value),
        validator3.validate(value),
        validator4.validate(value),
        validator5.validate(value),
        validator6.validate(value),
        validator7.validate(value),
        validator8.validate(value),
        validator9.validate(value),
        validator10.validate(value),
        validator11.validate(value),
        validator12.validate(value),
        validator13.validate(value),
        validator14.validate(value),
        validator15.validate(value),
        validator16.validate(value),
        validator17.validate(value),
        validator18.validate(value),
        validator19.validate(value),
      )(f)
  }

  private case class ValidateWith20[
      In,
      Out1,
      Out2,
      Out3,
      Out4,
      Out5,
      Out6,
      Out7,
      Out8,
      Out9,
      Out10,
      Out11,
      Out12,
      Out13,
      Out14,
      Out15,
      Out16,
      Out17,
      Out18,
      Out19,
      Out20,
      Out,
  ](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
      validator5: Validator[In, Out5],
      validator6: Validator[In, Out6],
      validator7: Validator[In, Out7],
      validator8: Validator[In, Out8],
      validator9: Validator[In, Out9],
      validator10: Validator[In, Out10],
      validator11: Validator[In, Out11],
      validator12: Validator[In, Out12],
      validator13: Validator[In, Out13],
      validator14: Validator[In, Out14],
      validator15: Validator[In, Out15],
      validator16: Validator[In, Out16],
      validator17: Validator[In, Out17],
      validator18: Validator[In, Out18],
      validator19: Validator[In, Out19],
      validator20: Validator[In, Out20],
  )(
      f: (
          Out1,
          Out2,
          Out3,
          Out4,
          Out5,
          Out6,
          Out7,
          Out8,
          Out9,
          Out10,
          Out11,
          Out12,
          Out13,
          Out14,
          Out15,
          Out16,
          Out17,
          Out18,
          Out19,
          Out20,
      ) => Out,
  ) extends Validator[In, Out] {
    override def validate(value: In): Validation[String, Out] =
      Validation.validateWith(
        validator1.validate(value),
        validator2.validate(value),
        validator3.validate(value),
        validator4.validate(value),
        validator5.validate(value),
        validator6.validate(value),
        validator7.validate(value),
        validator8.validate(value),
        validator9.validate(value),
        validator10.validate(value),
        validator11.validate(value),
        validator12.validate(value),
        validator13.validate(value),
        validator14.validate(value),
        validator15.validate(value),
        validator16.validate(value),
        validator17.validate(value),
        validator18.validate(value),
        validator19.validate(value),
        validator20.validate(value),
      )(f)
  }

  private case class ValidateWith21[
      In,
      Out1,
      Out2,
      Out3,
      Out4,
      Out5,
      Out6,
      Out7,
      Out8,
      Out9,
      Out10,
      Out11,
      Out12,
      Out13,
      Out14,
      Out15,
      Out16,
      Out17,
      Out18,
      Out19,
      Out20,
      Out21,
      Out,
  ](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
      validator5: Validator[In, Out5],
      validator6: Validator[In, Out6],
      validator7: Validator[In, Out7],
      validator8: Validator[In, Out8],
      validator9: Validator[In, Out9],
      validator10: Validator[In, Out10],
      validator11: Validator[In, Out11],
      validator12: Validator[In, Out12],
      validator13: Validator[In, Out13],
      validator14: Validator[In, Out14],
      validator15: Validator[In, Out15],
      validator16: Validator[In, Out16],
      validator17: Validator[In, Out17],
      validator18: Validator[In, Out18],
      validator19: Validator[In, Out19],
      validator20: Validator[In, Out20],
      validator21: Validator[In, Out21],
  )(
      f: (
          Out1,
          Out2,
          Out3,
          Out4,
          Out5,
          Out6,
          Out7,
          Out8,
          Out9,
          Out10,
          Out11,
          Out12,
          Out13,
          Out14,
          Out15,
          Out16,
          Out17,
          Out18,
          Out19,
          Out20,
          Out21,
      ) => Out,
  ) extends Validator[In, Out] {
    override def validate(value: In): Validation[String, Out] =
      Validation.validateWith(
        validator1.validate(value),
        validator2.validate(value),
        validator3.validate(value),
        validator4.validate(value),
        validator5.validate(value),
        validator6.validate(value),
        validator7.validate(value),
        validator8.validate(value),
        validator9.validate(value),
        validator10.validate(value),
        validator11.validate(value),
        validator12.validate(value),
        validator13.validate(value),
        validator14.validate(value),
        validator15.validate(value),
        validator16.validate(value),
        validator17.validate(value),
        validator18.validate(value),
        validator19.validate(value),
        validator20.validate(value),
        validator21.validate(value),
      )(f)
  }

  private case class ValidateWith22[
      In,
      Out1,
      Out2,
      Out3,
      Out4,
      Out5,
      Out6,
      Out7,
      Out8,
      Out9,
      Out10,
      Out11,
      Out12,
      Out13,
      Out14,
      Out15,
      Out16,
      Out17,
      Out18,
      Out19,
      Out20,
      Out21,
      Out22,
      Out,
  ](
      validator1: Validator[In, Out1],
      validator2: Validator[In, Out2],
      validator3: Validator[In, Out3],
      validator4: Validator[In, Out4],
      validator5: Validator[In, Out5],
      validator6: Validator[In, Out6],
      validator7: Validator[In, Out7],
      validator8: Validator[In, Out8],
      validator9: Validator[In, Out9],
      validator10: Validator[In, Out10],
      validator11: Validator[In, Out11],
      validator12: Validator[In, Out12],
      validator13: Validator[In, Out13],
      validator14: Validator[In, Out14],
      validator15: Validator[In, Out15],
      validator16: Validator[In, Out16],
      validator17: Validator[In, Out17],
      validator18: Validator[In, Out18],
      validator19: Validator[In, Out19],
      validator20: Validator[In, Out20],
      validator21: Validator[In, Out21],
      validator22: Validator[In, Out22],
  )(
      f: (
          Out1,
          Out2,
          Out3,
          Out4,
          Out5,
          Out6,
          Out7,
          Out8,
          Out9,
          Out10,
          Out11,
          Out12,
          Out13,
          Out14,
          Out15,
          Out16,
          Out17,
          Out18,
          Out19,
          Out20,
          Out21,
          Out22,
      ) => Out,
  ) extends Validator[In, Out] {
    override def validate(value: In): Validation[String, Out] =
      Validation.validateWith(
        validator1.validate(value),
        validator2.validate(value),
        validator3.validate(value),
        validator4.validate(value),
        validator5.validate(value),
        validator6.validate(value),
        validator7.validate(value),
        validator8.validate(value),
        validator9.validate(value),
        validator10.validate(value),
        validator11.validate(value),
        validator12.validate(value),
        validator13.validate(value),
        validator14.validate(value),
        validator15.validate(value),
        validator16.validate(value),
        validator17.validate(value),
        validator18.validate(value),
        validator19.validate(value),
        validator20.validate(value),
        validator21.validate(value),
        validator22.validate(value),
      )(f)
  }

}
