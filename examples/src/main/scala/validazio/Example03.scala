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
      listOfChildren: Option[List[Child]] = None,
      setOfChildren: Option[Set[Child]] = None,
      mapOfChildren: Option[Map[String, Child]] = None,
  )

  case class ParentValid(
      requiredChild: ChildValid,
      optionalChild: Option[ChildValid],
      listOfChildren: List[ChildValid],
      setOfChildren: Set[ChildValid],
      mapOfChildren: Map[String, ChildValid],
  )

  given Validator[Parent, ParentValid] = {
    val requiredChildValidator: Validator[Parent, ChildValid] = labeled("requiredChild") {
      required >> valid[Child, ChildValid]
    }.contraMap[Parent](_.requiredChild)

    val optionalChildValidator: Validator[Parent, Option[ChildValid]] = labeled("optionalChild") {
      valid[Child, ChildValid].optional
    }.contraMap[Parent](_.optionalChild)

    val listOfChildrenValidator: Validator[Parent, List[ChildValid]] = labeled("listOfChildren") {
      valid[Child, ChildValid].list.optional.map(_.getOrElse(List.empty))
    }.contraMap[Parent](_.listOfChildren)

    val setOfChildrenValidator: Validator[Parent, Set[ChildValid]] = labeled("setOfChildren") {
      valid[Child, ChildValid].set.optional.map(_.getOrElse(Set.empty))
    }.contraMap[Parent](_.setOfChildren)

    val mapOfChildrenValidator: Validator[Parent, Map[String, ChildValid]] = labeled("mapOfChildren") {
      valid[Child, ChildValid].keyValuePairs[String].optional.map(_.getOrElse(Map.empty))
    }.contraMap[Parent](_.mapOfChildren)

    (
      requiredChildValidator
        ++ optionalChildValidator
        ++ listOfChildrenValidator
        ++ setOfChildrenValidator
        ++ mapOfChildrenValidator
    ).map(ParentValid.apply)
  }

  override def run: ZIO[ZIOAppArgs & Scope, Any, Any] = {
    val child1 = Child(name = Some("Ala"))
    val child2 = Child(name = Some("Ela"))
    val child3 = Child(name = Some("Ola"))
    val child4 = Child(name = Some("Ula"))
    val parent = Parent(
      requiredChild = Some(child1),
      optionalChild = Some(child2),
      listOfChildren = Some(List(child3, child4)),
      setOfChildren = Some(Set(child3, child4)),
      mapOfChildren = Some(
        Map(
          "Ala" -> child1,
          "Ela" -> child2,
          "Ola" -> child3,
          "Ula" -> child4,
        ),
      ),
    )

    for {
      parentValid <- validateZIO(ValidationException.apply)(parent).exit
      _           <- ZIO.log(s"parentValid: $parentValid")
    } yield ()
  }
}
