ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.4"

lazy val root = (project in file("."))
  .settings(
    name := "AdventOfCode2024",
    libraryDependencies += "org.scala-lang" %% "toolkit" % "0.1.7"
  )
