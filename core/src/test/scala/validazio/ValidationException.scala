package validazio

import zio.prelude.Associative

case class ValidationException(messages: List[String]) extends RuntimeException {
  override def getMessage: String = messages.mkString(", ")
}

object ValidationException {
  def apply(message: String): ValidationException = ValidationException(List(message))

  given Associative[ValidationException] {
    override def combine(l: => ValidationException, r: => ValidationException): ValidationException =
      ValidationException(l.messages ++ r.messages)
  }
}
