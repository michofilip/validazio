package validazio

import izumi.reflect.Tag
import zio.prelude.PartialOrd

import scala.reflect.ClassTag

enum ValidationFailure {
  case Condition(description: String)
  case Required(label: Label)
  case IsTrue(label: Label)
  case IsFalse(label: Label)
  case Min[T: PartialOrd](label: Label, min: T, inclusive: Boolean)
  case Max[T: PartialOrd](label: Label, max: T, inclusive: Boolean)
  case MinSize(label: Label, min: Int, inclusive: Boolean)
  case MaxSize(label: Label, max: Int, inclusive: Boolean)
  case MinSetSize(label: Label, min: Int, inclusive: Boolean)
  case MaxSetSize(label: Label, max: Int, inclusive: Boolean)
  case NotEmpty(label: Label)
  case NotBlank(label: Label)
  case MinLength(label: Label, min: Int, inclusive: Boolean)
  case MaxLength(label: Label, max: Int, inclusive: Boolean)
  case RegExr(label: Label, regex: String, description: String)
}
