package validazio

import validazio.*
import zio.*

object Example03 extends ZIOAppDefault {

  case class Child(
      name: Option[String] = None,
  )

  case class ChildValid(
      name: String,
  )

  given Validator[Child, ChildValid] = {
    val nameValidator: Validator[Child, String] = labeled("name") {
      required
    }.contraMap[Child](_.name)

    nameValidator.map(ChildValid.apply)
  }

  case class Parent(
      requiredChild: Option[Child] = None,
      optionalChild: Option[Child] = None,
      children: Option[List[Child]] = None,
  )

  case class ParentValid(
      requiredChild: ChildValid,
      optionalChild: Option[ChildValid],
      children: List[ChildValid],
  )

  given Validator[Parent, ParentValid] = {
    val requiredChildValidator: Validator[Parent, ChildValid] = labeled("requiredChild") {
      required >> valid[Child, ChildValid]
    }.contraMap[Parent](_.requiredChild)

    val optionalChildValidator: Validator[Parent, Option[ChildValid]] = labeled("optionalChild") {
      valid[Child, ChildValid].optional
    }.contraMap[Parent](_.optionalChild)

    val childrenValidator: Validator[Parent, List[ChildValid]] = labeled("children") {
      valid[Child, ChildValid].list.optional.map(_.getOrElse(List.empty))
    }.contraMap[Parent](_.children)

    validateWith(
      requiredChildValidator,
      optionalChildValidator,
      childrenValidator,
    )(ParentValid.apply)
  }

  override def run: ZIO[ZIOAppArgs & Scope, Any, Any] = {
    val child1 = Child(name = Some("Ala"))
    val child2 = Child(name = Some("Ela"))
    val child3 = Child(name = Some("Ola"))
    val child4 = Child(name = Some("Ula"))
    val parent = Parent(
      requiredChild = Some(child1),
      optionalChild = Some(child2),
      children = Some(List(child3, child4)),
    )

    for {
      parentValid <- Validator.validateZIO(ValidationException.apply)(parent).exit
      _           <- ZIO.log(s"parentValid: $parentValid")
    } yield ()
  }
}
