package validazio

import validazio.*
import zio.*

object Example01 extends ZIOAppDefault {

  case class User(
      username: String,
      password: String,
      age: Int,
      accepted: Boolean,
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
        regExr("[@#$%&_]", "must contain a special character"),
        regExr("^\\S*$", "must not contain a whitespace character"),
      )
    }.contraMap(_.password)

    val ageValidator: Validator[User, Unit] = labeled("age") {
      allDiscard(
        min(18),
        max(100, inclusive = false),
      )
    }.contraMap(_.age)

    val acceptedValidator: Validator[User, Unit] = labeled("accepted") {
      isTrue.unit
    }.contraMap(_.accepted)

    id << allDiscard(
      usernameValidator,
      passwordValidator,
      ageValidator,
      acceptedValidator,
    )
  }

  override def run: ZIO[ZIOAppArgs & Scope, Any, Any] = {
    val user = User(
      username = "username",
      password = "Pa$$w0rd",
      age = 20,
      accepted = true,
    )

    val userIncorrect = User(
      username = "",
      password = "",
      age = 0,
      accepted = false,
    )

    for {
      user          <- validateZIO(ValidationException.apply)(user).exit
      _             <- ZIO.log(s"user: $user")
      userIncorrect <- validateZIO(ValidationException.apply)(userIncorrect).exit
      _             <- ZIO.log(s"userIncorrect: $userIncorrect")
    } yield ()
  }
}
