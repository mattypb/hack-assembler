name := "hack-assembler"

version := "0.1"

scalaVersion := "2.13.3"

libraryDependencies ++= Seq(
  "co.fs2" %% "fs2-core" % "2.4.4",
  "co.fs2" %% "fs2-io" % "2.4.4",
  "org.typelevel" %% "cats-effect" % "2.1.4",
  "org.scalatest" %% "scalatest" % "3.2.2" % Test
)

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:postfixOps",
  "-language:higherKinds"
)