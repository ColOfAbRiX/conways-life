name := "conways-life"

description := "Conway's Game of Life"

version := "1.0"

scalaVersion := "2.12.8"

mainClass in (Compile, run) := Some("com.colofabrix.scala.ConwaysLife")

// Cats
scalacOptions += "-Ypartial-unification"
libraryDependencies += "org.typelevel" %% "cats-core" % "1.5.0"