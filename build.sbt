name := "scala-practice"

version := "0.0.1"

scalaVersion := "2.12.7"

scalacOptions ++=
  Seq(
    "-Ypartial-unification"
  )

libraryDependencies ++=
  Seq(
    "org.typelevel"   %%  "cats-core"   %  "1.4.0",
    "org.scalacheck"  %%  "scalacheck"  %  "1.14.0"     % "test",
    "org.scalatest"   %%  "scalatest"   %  "3.0.5"      % "test"
  )
