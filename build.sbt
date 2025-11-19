ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.3"

lazy val root = (project in file("."))
  .settings(
    name := "chemistry-equations-dsl",
    scalacOptions += "-language:implicitConversions",
    libraryDependencies += "org.junit.jupiter" % "junit-jupiter-api" % "6.1.0-M1" % Test
  )
