ThisBuild / version := "0.1.0"

ThisBuild / scalaVersion := "3.2.2"

lazy val root = (project in file("."))
  .settings(
    name := "PicoUnify",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0-M7" % Test
  )
