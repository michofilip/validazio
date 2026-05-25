package validazio

import validazio.Validator
import zio.*
import zio.test.*

object ValidatorSpec extends ZIOSpecDefault {
  override def spec: Spec[TestEnvironment & Scope, Any] = {
    given Label("value")

    suite("validazio")(
      suite("provides function")(
        suite("'id' that")(
          test("succeeds with the same value") {
            given Validator[Int, Int] = id

            for {
              res <- validateZIO(ValidationException.apply)(42).exit
            } yield assertTrue {
              res == Exit.succeed(42)
            }
          },
        ),
        suite("'required' that")(
          test("succeeds with content of an option if Some") {
            given Validator[Option[Int], Int] = required

            for {
              res <- validateZIO(ValidationException.apply)(Option(42)).exit
            } yield assertTrue {
              res == Exit.succeed(42)
            }
          },
          test("fails if None") {
            given Validator[Option[Int], Int] = required

            for {
              res <- validateZIO(ValidationException.apply)(Option.empty[Int]).exit
            } yield assertTrue {
              res == Exit.fail(ValidationException("value is required"))
            }
          },
        ),
        suite("'condition' that")(
          test("succeeds if predicate is true") {
            given Validator[Int, Int] = condition(_ == 42, "must be 42")

            for {
              res <- validateZIO(ValidationException.apply)(42).exit
            } yield assertTrue {
              res == Exit.succeed(42)
            }
          },
          test("fails if predicate is false") {
            given Validator[Int, Int] = condition(_ == 42, "value must be 42")

            for {
              res <- validateZIO(ValidationException.apply)(0).exit
            } yield assertTrue {
              res == Exit.fail(ValidationException("value must be 42"))
            }
          },
        ),
        suite("'min' that")(
          suite("when parameter `inclusive` is true")(
            test("succeeds if value is greater than min value") {
              given Validator[Int, Int] = min(0)

              for {
                res <- validateZIO(ValidationException.apply)(1).exit
              } yield assertTrue {
                res == Exit.succeed(1)
              }
            },
            test("succeeds if value is equal to min value") {
              given Validator[Int, Int] = min(0)

              for {
                res <- validateZIO(ValidationException.apply)(0).exit
              } yield assertTrue {
                res == Exit.succeed(0)
              }
            },
            test("fails if value is smaller than min value") {
              given Validator[Int, Int] = min(0)

              for {
                res <- validateZIO(ValidationException.apply)(-1).exit
              } yield assertTrue {
                res == Exit.fail(ValidationException("value must be more then or equal to 0"))
              }
            },
          ),
          suite("when parameter `inclusive` is false")(
            test("succeeds if value is greater than min value") {
              given Validator[Int, Int] = min(0, inclusive = false)

              for {
                res <- validateZIO(ValidationException.apply)(1).exit
              } yield assertTrue {
                res == Exit.succeed(1)
              }
            },
            test("fails if value is equal to min value") {
              given Validator[Int, Int] = min(0, inclusive = false)

              for {
                res <- validateZIO(ValidationException.apply)(0).exit
              } yield assertTrue {
                res == Exit.fail(ValidationException("value must be more then 0"))
              }
            },
            test("fails if value is smaller than min value") {
              given Validator[Int, Int] = min(0, inclusive = false)

              for {
                res <- validateZIO(ValidationException.apply)(-1).exit
              } yield assertTrue {
                res == Exit.fail(ValidationException("value must be more then 0"))
              }
            },
          ),
        ),
        suite("'max' that")(
          suite("when parameter `inclusive` is true")(
            test("succeeds if value is smaller than max value") {
              given Validator[Int, Int] = max(0)

              for {
                res <- validateZIO(ValidationException.apply)(-1).exit
              } yield assertTrue {
                res == Exit.succeed(-1)
              }
            },
            test("succeeds if value is equal to max value") {
              given Validator[Int, Int] = max(0)

              for {
                res <- validateZIO(ValidationException.apply)(0).exit
              } yield assertTrue {
                res == Exit.succeed(0)
              }
            },
            test("fails if value is greater than max value") {
              given Validator[Int, Int] = max(0)

              for {
                res <- validateZIO(ValidationException.apply)(1).exit
              } yield assertTrue {
                res == Exit.fail(ValidationException("value must be less then or equal to 0"))
              }
            },
          ),
          suite("when parameter `inclusive` is false")(
            test("succeeds if value is greater than max value") {
              given Validator[Int, Int] = max(0, inclusive = false)

              for {
                res <- validateZIO(ValidationException.apply)(-1).exit
              } yield assertTrue {
                res == Exit.succeed(-1)
              }
            },
            test("fails if value is equal to max value") {
              given Validator[Int, Int] = max(0, inclusive = false)

              for {
                res <- validateZIO(ValidationException.apply)(0).exit
              } yield assertTrue {
                res == Exit.fail(ValidationException("value must be less then 0"))
              }
            },
            test("fails if value is smaller than max value") {
              given Validator[Int, Int] = max(0, inclusive = false)

              for {
                res <- validateZIO(ValidationException.apply)(1).exit
              } yield assertTrue {
                res == Exit.fail(ValidationException("value must be less then 0"))
              }
            },
          ),
        ),
        suite("'notEmpty' that")(
          test("succeeds if value is not empty string") {
            given Validator[String, String] = notEmpty

            for {
              res <- validateZIO(ValidationException.apply)("test").exit
            } yield assertTrue {
              res == Exit.succeed("test")
            }
          },
          test("fails if value is empty string") {
            given Validator[String, String] = notEmpty

            for {
              res <- validateZIO(ValidationException.apply)("").exit
            } yield assertTrue {
              res == Exit.fail(ValidationException("value must not be empty"))
            }
          },
        ),
        suite("'notBlank' that")(
          test("succeeds if value is not blank string") {
            given Validator[String, String] = notBlank

            for {
              res <- validateZIO(ValidationException.apply)("test").exit
            } yield assertTrue {
              res == Exit.succeed("test")
            }
          },
          test("fails if value is blank string") {
            given Validator[String, String] = notBlank

            for {
              res <- validateZIO(ValidationException.apply)("   ").exit
            } yield assertTrue {
              res == Exit.fail(ValidationException("value must not be blank"))
            }
          },
          test("fails if value is empty string") {
            given Validator[String, String] = notBlank

            for {
              res <- validateZIO(ValidationException.apply)("").exit
            } yield assertTrue {
              res == Exit.fail(ValidationException("value must not be blank"))
            }
          },
        ),
        suite("'minLength' that")(
          suite("when parameter `inclusive` is true")(
            test("succeeds if value is a string with length greater than min length") {
              given Validator[String, String] = minLength(3)

              for {
                res <- validateZIO(ValidationException.apply)("test").exit
              } yield assertTrue {
                res == Exit.succeed("test")
              }
            },
            test("succeeds if value is a string with length equal to min length") {
              given Validator[String, String] = minLength(4)

              for {
                res <- validateZIO(ValidationException.apply)("test").exit
              } yield assertTrue {
                res == Exit.succeed("test")
              }
            },
            test("fails if value is a string with length smaller than min length") {
              given Validator[String, String] = minLength(5)

              for {
                res <- validateZIO(ValidationException.apply)("test").exit
              } yield assertTrue {
                res == Exit.fail(ValidationException("value length must be longer then or equal to 5"))
              }
            },
          ),
          suite("when parameter `inclusive` is false")(
            test("succeeds if value is a string with length greater than min length") {
              given Validator[String, String] = minLength(3, inclusive = false)

              for {
                res <- validateZIO(ValidationException.apply)("test").exit
              } yield assertTrue {
                res == Exit.succeed("test")
              }
            },
            test("fails if value is a string with length equal to min length") {
              given Validator[String, String] = minLength(4, inclusive = false)

              for {
                res <- validateZIO(ValidationException.apply)("test").exit
              } yield assertTrue {
                res == Exit.fail(ValidationException("value length must be longer then 4"))
              }
            },
            test("fails if value is a string with length smaller than min length") {
              given Validator[String, String] = minLength(5, inclusive = false)

              for {
                res <- validateZIO(ValidationException.apply)("test").exit
              } yield assertTrue {
                res == Exit.fail(ValidationException("value length must be longer then 5"))
              }
            },
          ),
        ),
        suite("'maxLength' that")(
          suite("when parameter `inclusive` is true")(
            test("succeeds if value is a string with length smaller than max length") {
              given Validator[String, String] = maxLength(5)

              for {
                res <- validateZIO(ValidationException.apply)("test").exit
              } yield assertTrue {
                res == Exit.succeed("test")
              }
            },
            test("succeeds if value is a string with length equal to max length") {
              given Validator[String, String] = maxLength(4)

              for {
                res <- validateZIO(ValidationException.apply)("test").exit
              } yield assertTrue {
                res == Exit.succeed("test")
              }
            },
            test("fails if value is a string with length greater than max length") {
              given Validator[String, String] = maxLength(3)

              for {
                res <- validateZIO(ValidationException.apply)("test").exit
              } yield assertTrue {
                res == Exit.fail(ValidationException("value length must be shorter then or equal to 3"))
              }
            },
          ),
          suite("when parameter `inclusive` is false")(
            test("succeeds if value is a string with length smaller than max length") {
              given Validator[String, String] = maxLength(5, inclusive = false)

              for {
                res <- validateZIO(ValidationException.apply)("test").exit
              } yield assertTrue {
                res == Exit.succeed("test")
              }
            },
            test("fails if value is a string with length equal to max length") {
              given Validator[String, String] = maxLength(4, inclusive = false)

              for {
                res <- validateZIO(ValidationException.apply)("test").exit
              } yield assertTrue {
                res == Exit.fail(ValidationException("value length must be shorter then 4"))
              }
            },
            test("fails if value is a string with length greater than max length") {
              given Validator[String, String] = maxLength(3, inclusive = false)

              for {
                res <- validateZIO(ValidationException.apply)("test").exit
              } yield assertTrue {
                res == Exit.fail(ValidationException("value length must be shorter then 3"))
              }
            },
          ),
        ),
        suite("'regExr' that")(
          test("succeeds if value is a string that satisfies regex") {
            given Validator[String, String] = regExr("[0-9]", "must contain a digit")

            for {
              res <- validateZIO(ValidationException.apply)("test1").exit
            } yield assertTrue {
              res == Exit.succeed("test1")
            }
          },
          test("fails if value is a string that does not satisfy regex") {
            given Validator[String, String] = regExr("[0-9]", "must contain a digit")

            for {
              res <- validateZIO(ValidationException.apply)("test").exit
            } yield assertTrue {
              res == Exit.fail(ValidationException("value must contain a digit"))
            }
          },
        ),
        suite("'all' that")(
          test("succeeds if all validators succeed") {
            given Validator[String, List[String]] = all(
              regExr("[a-z]", "must contain a lowercase character"),
              regExr("[A-Z]", "must contain an uppercase character"),
              regExr("[0-9]", "must contain a digit"),
            )

            for {
              res <- validateZIO(ValidationException.apply)("Test1").exit
            } yield assertTrue {
              res == Exit.succeed(List("Test1", "Test1", "Test1"))
            }
          },
          test("fails if any of validators fail") {
            given Validator[String, List[String]] = all(
              regExr("[a-z]", "must contain a lowercase character"),
              regExr("[A-Z]", "must contain an uppercase character"),
              regExr("[0-9]", "must contain a digit"),
            )

            for {
              res <- validateZIO(ValidationException.apply)("test").exit
            } yield assertTrue {
              res == Exit.fail(
                ValidationException(
                  List(
                    "value must contain an uppercase character",
                    "value must contain a digit",
                  ),
                ),
              )
            }
          },
        ),
        suite("'allDiscard' that")(
          test("succeeds if all validators succeed and discards results") {
            given Validator[String, Unit] = allDiscard(
              regExr("[a-z]", "must contain a lowercase character"),
              regExr("[A-Z]", "must contain an uppercase character"),
              regExr("[0-9]", "must contain a digit"),
            )

            for {
              res <- validateZIO(ValidationException.apply)("Test1").exit
            } yield assertTrue {
              res == Exit.succeed(())
            }
          },
          test("fails if any of validators fail") {
            given Validator[String, Unit] = allDiscard(
              regExr("[a-z]", "must contain a lowercase character"),
              regExr("[A-Z]", "must contain an uppercase character"),
              regExr("[0-9]", "must contain a digit"),
            )

            for {
              res <- validateZIO(ValidationException.apply)("test").exit
            } yield assertTrue {
              res == Exit.fail(
                ValidationException(
                  List(
                    "value must contain an uppercase character",
                    "value must contain a digit",
                  ),
                ),
              )
            }
          },
        ),
      ),
    )
  }
}
