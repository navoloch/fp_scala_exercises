import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.navoloch",
      scalaVersion := "2.12.4",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "fp_scala",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "junit" % "junit" % "4.10" % Test

)
