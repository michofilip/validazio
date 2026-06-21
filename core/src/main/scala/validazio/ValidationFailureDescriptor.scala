package validazio

trait ValidationFailureDescriptor[T] {
  def describe(validationFailure: ValidationFailure): T
}

object ValidationFailureDescriptor {

  def defaultDescriptor: ValidationFailureDescriptor[String] = {
    case ValidationFailure.Condition(description) =>
      description

    case ValidationFailure.Required(label) =>
      s"${label.label} is required"

    case ValidationFailure.IsTrue(label) =>
      s"${label.label} must be true"

    case ValidationFailure.IsFalse(label) =>
      s"${label.label} must be false"

    case ValidationFailure.Min(label, min, inclusive) =>
      if (inclusive) s"${label.label} must be more then or equal to $min"
      else s"${label.label} must be more then $min"

    case ValidationFailure.Max(label, max, inclusive) =>
      if (inclusive) s"${label.label} must be less then or equal to $max"
      else s"${label.label} must be less then $max"

    case ValidationFailure.MinSize(label, minSize, inclusive) =>
      if (inclusive) s"${label.label} size must be more then or equal to $minSize"
      else s"${label.label} size must be more then $minSize"

    case ValidationFailure.MaxSize(label, maxSize, inclusive) =>
      if (inclusive) s"${label.label} size must be less then or equal to $maxSize"
      else s"${label.label} size must be less then $maxSize"

    case ValidationFailure.MinSetSize(label, minSize, inclusive) =>
      if (inclusive) s"${label.label} size must be more then or equal to $minSize"
      else s"${label.label} size must be more then $minSize"

    case ValidationFailure.MaxSetSize(label, maxSize, inclusive) =>
      if (inclusive) s"${label.label} size must be less then or equal to $maxSize"
      else s"${label.label} size must be less then $maxSize"

    case ValidationFailure.NotEmpty(label) =>
      s"${label.label} must not be empty"

    case ValidationFailure.NotBlank(label) =>
      s"${label.label} must not be blank"

    case ValidationFailure.MinLength(label, minLength, inclusive) =>
      if (inclusive) s"${label.label} length must be longer then or equal to $minLength"
      else s"${label.label} length must be longer then $minLength"

    case ValidationFailure.MaxLength(label, maxLength, inclusive) =>
      if (inclusive) s"${label.label} length must be shorter then or equal to $maxLength"
      else s"${label.label} length must be shorter then $maxLength"

    case ValidationFailure.RegExr(label, regex, description) =>
      s"${label.label} $description"
  }

}
