val V = new {
  val Scala      = "3.2.1"
  val ScalaGroup = "3.2"

  val catsEffect = "3.4.3"

  val scribe          = "3.10.5"
  val hedgehog        = "0.10.1"
  val organiseImports = "0.6.0"
  val zerowaste       = "0.2.1"

}

val Dependencies = new {
  lazy val aoc = Seq(
    libraryDependencies ++= Seq(
      "org.typelevel"   %% "cats-effect"  % V.catsEffect,
      "com.outr"        %% "scribe-slf4j" % V.scribe,
      "com.github.ghik" %% "zerowaste"    % V.zerowaste cross CrossVersion.full,
    ),
  )

  lazy val tests = Def.settings(
    libraryDependencies ++= Seq(
      "qa.hedgehog" %% "hedgehog-munit" % V.hedgehog % Test,
    ),
    Test / fork := true,
  )
}

ThisBuild / organization := "dev.sungkm"
ThisBuild / version      := "0.0.1-SNAPSHOT"
ThisBuild / scalaVersion := V.Scala
ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % V.organiseImports
ThisBuild / semanticdbEnabled := true

lazy val aoc = (project in file("."))
  .settings(Dependencies.aoc)
  .settings(Dependencies.tests)
  .settings(
    name := "sungkm-advent-of-code-2022",
  )
