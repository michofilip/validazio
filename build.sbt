ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.8.3"

val zioVersion        = "2.1.26"
val zioPreludeVersion = "1.0.0-RC47"

lazy val root = (project in file("."))
  .settings(
    name           := "validazio",
    publish / skip := true,
  )
  .aggregate(core, examples)

lazy val core = project.settings(
  name := "validazio-core",
  libraryDependencies ++= Seq(
    "dev.zio" %% "zio"         % zioVersion,
    "dev.zio" %% "zio-test"    % zioVersion % Test,
    "dev.zio" %% "zio-prelude" % zioPreludeVersion,
  ),
)

lazy val examples = project
  .settings(
    publish / skip := true,
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"         % zioVersion,
      "dev.zio" %% "zio-test"    % zioVersion % Test,
      "dev.zio" %% "zio-prelude" % zioPreludeVersion,
    ),
  )
  .dependsOn(core)
