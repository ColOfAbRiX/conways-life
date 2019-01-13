name := "scala"

version := "0.1"

scalaVersion := "2.12.8"

scalacOptions += "-Ypartial-unification"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.5.0",
  "org.typelevel" %% "cats-kernel" % "1.5.0",
  "org.typelevel" %% "cats-macros" % "1.5.0"
)

resolvers += Resolver.sonatypeRepo("releases")
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8")
