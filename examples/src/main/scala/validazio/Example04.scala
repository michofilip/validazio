package validazio

import validazio.*
import zio.*

object Example04 extends ZIOAppDefault {

  case class MoL(
      meaningOfLifePart1: Int,
      meaningOfLifePart2: Int,
  )

  given Validator[MoL, Int] = {
    id[MoL].map(mol => mol.meaningOfLifePart1 + mol.meaningOfLifePart2) >>
      condition(_ == 42, "meaning of life must be 42")
  }

  override def run: ZIO[ZIOAppArgs & Scope, Any, Any] = {
    val mol = MoL(
      meaningOfLifePart1 = 40,
      meaningOfLifePart2 = 2,
    )

    val molIncorrect = MoL(
      meaningOfLifePart1 = 0,
      meaningOfLifePart2 = 0,
    )

    for {
      mol          <- Validator.validateZIO(ValidationException.apply)(mol).exit
      _            <- ZIO.log(s"mol: $mol")
      molIncorrect <- Validator.validateZIO(ValidationException.apply)(molIncorrect).exit
      _            <- ZIO.log(s"molIncorrect: $molIncorrect")
    } yield ()
  }
}
