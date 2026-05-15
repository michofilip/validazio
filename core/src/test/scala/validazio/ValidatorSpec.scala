package validazio

import validazio.Validator
import validazio.Validator.*
import zio.*
import zio.test.*

object ValidatorSpec extends ZIOSpecDefault {
  override def spec: Spec[TestEnvironment & Scope, Any] = {
    suite("Validator")(
      suite("provides function 'id' that")(
        test("succeeds with the same value") {
          given Validator[Int, Int] = id

          for {
            res <- Validator.validateZIO(ValidationException.apply)(42).exit
          } yield assertTrue {
            res == Exit.succeed(42)
          }
        },
      ),
      suite("provides function 'required' that")(
        test("succeeds with content of an option if Some") {
          given Validator[Option[Int], Int] = labeled("Int")(required)

          for {
            res <- Validator.validateZIO(ValidationException.apply)(Option(42)).exit
          } yield assertTrue {
            res == Exit.succeed(42)
          }
        },
        test("fails if None") {
          given Validator[Option[Int], Int] = labeled("Int")(required)

          for {
            res <- Validator.validateZIO(ValidationException.apply)(Option.empty[Int]).exit
          } yield assertTrue {
            res == Exit.fail(ValidationException("Int is required"))
          }
        },
      ),
    )
  }
}
