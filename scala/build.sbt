name := "conways-life"

description := "Conway's Game of Life"

version := "1.0"

scalaVersion := "2.12.8"

mainClass in (Compile, run) := Some("com.colofabrix.scala.ConwaysLife")

// Cats
scalacOptions += "-Ypartial-unification"
libraryDependencies += "org.typelevel" %% "cats-core" % "1.5.0"

// Kind Projector
resolvers += Resolver.sonatypeRepo("releases")
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8")

// Command line parser
libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.0-RC2"
