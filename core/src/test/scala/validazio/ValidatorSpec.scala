package validazio

import validazio.Validator
import validazio.Validator.*
import zio.*
import zio.test.*

object ValidatorSpec extends ZIOSpecDefault {
  override def spec: Spec[TestEnvironment & Scope, Any] = {
    given Label("value")

    suite("Validator")(
      suite("provides function 'id' that")(
        test("succeeds with the same value") {
          given Validator[Int, Int] = id

          for {
            res <- Validator
              .validateZIO(ValidationException.apply)(42)
              .exit
          } yield assertTrue {
            res == Exit.succeed(42)
          }
        },
      ),
      suite("provides function 'required' that")(
        test("succeeds with content of an option if Some") {
          given Validator[Option[Int], Int] = required

          for {
            res <- Validator.validateZIO(ValidationException.apply)(Option(42)).exit
          } yield assertTrue {
            res == Exit.succeed(42)
          }
        },
        test("fails if None") {
          given Validator[Option[Int], Int] = required

          for {
            res <- Validator.validateZIO(ValidationException.apply)(Option.empty[Int]).exit
          } yield assertTrue {
            res == Exit.fail(ValidationException("value is required"))
          }
        },
      ),
      suite("provides function 'condition' that")(
        test("succeeds if predicate is true") {
          given Validator[Int, Int] = condition(_ == 42, "must be 42")

          for {
            res <- Validator.validateZIO(ValidationException.apply)(42).exit
          } yield assertTrue {
            res == Exit.succeed(42)
          }
        },
        test("fails if predicate is false") {
          given Validator[Int, Int] = condition(_ == 42, "value must be 42")

          for {
            res <- Validator.validateZIO(ValidationException.apply)(0).exit
          } yield assertTrue {
            res == Exit.fail(ValidationException("value must be 42"))
          }
        },
      ),
      suite("provides function 'min' that")(
        suite("when parameter `inclusive` is true")(
          test("succeeds if value is greater than min value") {
            given Validator[Int, Int] = min(0)

            for {
              res <- Validator.validateZIO(ValidationException.apply)(1).exit
            } yield assertTrue {
              res == Exit.succeed(1)
            }
          },
          test("succeeds if value is equal to min value") {
            given Validator[Int, Int] = min(0)

            for {
              res <- Validator.validateZIO(ValidationException.apply)(0).exit
            } yield assertTrue {
              res == Exit.succeed(0)
            }
          },
          test("fails if value is less than min value") {
            given Validator[Int, Int] = min(0)

            for {
              res <- Validator.validateZIO(ValidationException.apply)(-1).exit
            } yield assertTrue {
              res == Exit.fail(ValidationException("value must be more then or equal to 0"))
            }
          },
        ),
        suite("when parameter `inclusive` is false")(
          test("succeeds if value is greater than min value") {
            given Validator[Int, Int] = min(0, inclusive = false)

            for {
              res <- Validator.validateZIO(ValidationException.apply)(1).exit
            } yield assertTrue {
              res == Exit.succeed(1)
            }
          },
          test("fails if value is equal to min value") {
            given Validator[Int, Int] = min(0, inclusive = false)

            for {
              res <- Validator.validateZIO(ValidationException.apply)(0).exit
            } yield assertTrue {
              res == Exit.fail(ValidationException("value must be more then 0"))
            }
          },
          test("fails if value is less than min value") {
            given Validator[Int, Int] = min(0, inclusive = false)

            for {
              res <- Validator.validateZIO(ValidationException.apply)(-1).exit
            } yield assertTrue {
              res == Exit.fail(ValidationException("value must be more then 0"))
            }
          },
        ),
      ),
      suite("provides function 'max' that")(
        suite("when parameter `inclusive` is true")(
          test("succeeds if value is less than max value") {
            given Validator[Int, Int] = max(0)

            for {
              res <- Validator.validateZIO(ValidationException.apply)(-1).exit
            } yield assertTrue {
              res == Exit.succeed(-1)
            }
          },
          test("succeeds if value is equal to max value") {
            given Validator[Int, Int] = max(0)

            for {
              res <- Validator.validateZIO(ValidationException.apply)(0).exit
            } yield assertTrue {
              res == Exit.succeed(0)
            }
          },
          test("fails if value is more than max value") {
            given Validator[Int, Int] = max(0)

            for {
              res <- Validator.validateZIO(ValidationException.apply)(1).exit
            } yield assertTrue {
              res == Exit.fail(ValidationException("value must be less then or equal to 0"))
            }
          },
        ),
        suite("when parameter `inclusive` is false")(
          test("succeeds if value is greater than max value") {
            given Validator[Int, Int] = max(0, inclusive = false)

            for {
              res <- Validator.validateZIO(ValidationException.apply)(-1).exit
            } yield assertTrue {
              res == Exit.succeed(-1)
            }
          },
          test("fails if value is equal to max value") {
            given Validator[Int, Int] = max(0, inclusive = false)

            for {
              res <- Validator.validateZIO(ValidationException.apply)(0).exit
            } yield assertTrue {
              res == Exit.fail(ValidationException("value must be less then 0"))
            }
          },
          test("fails if value is less than max value") {
            given Validator[Int, Int] = max(0, inclusive = false)

            for {
              res <- Validator.validateZIO(ValidationException.apply)(1).exit
            } yield assertTrue {
              res == Exit.fail(ValidationException("value must be less then 0"))
            }
          },
        ),
      )
    )
  }
}
