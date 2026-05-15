package validazio

import validazio.Validator.*
import zio.*

object Example01 extends ZIOAppDefault {

  case class User(
      username: String,
      password: String,
      age: Int,
  )

  given Validator[User, User] = {
    val usernameValidator: Validator[User, Unit] = labeled("username") {
      allDiscard(
        notBlank,
        minLength(3),
        maxLength(20, inclusive = false),
      )
    }.contraMap(_.username)

    val passwordValidator: Validator[User, Unit] = labeled("password") {
      allDiscard(
        minLength(8),
        regExr("[a-z]", "must contain a lowercase character"),
        regExr("[A-Z]", "must contain an uppercase character"),
        regExr("[0-9]", "must contain a digit"),
        regExr("^\\S*$", "must not contain a whitespace character"),
      )
    }.contraMap(_.password)

    val ageValidator: Validator[User, Unit] = labeled("age") {
      allDiscard(
        min(18),
        max(100, inclusive = false),
      )
    }.contraMap(_.age)

    id << allDiscard(
      usernameValidator,
      passwordValidator,
      ageValidator,
    )
  }

  override def run: ZIO[ZIOAppArgs & Scope, Any, Any] = {
    val user = User(
      username = "username",
      password = "paZZw0rd",
      age = 20,
    )

    val userIncorrect = User(
      username = "",
      password = "",
      age = 0,
    )

    for {
      user          <- Validator.validateZIO(ValidationException.apply)(user).exit
      _             <- ZIO.log(s"user: $user")
      userIncorrect <- Validator.validateZIO(ValidationException.apply)(userIncorrect).exit
      _             <- ZIO.log(s"userIncorrect: $userIncorrect")
    } yield ()
  }
}
