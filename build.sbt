name := "sequences"

organization := "AMP"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.12"

crossScalaVersions := Seq("2.10.4", "2.11.12")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.5" % "test"
)

