name := "MyInterpreter"

version := "0.1"

scalaVersion := "2.13.4"


libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.2" % "test"
)

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % "2.13.2",
      "org.scalatest" %% "scalatest" % "3.1.1" % Test,
    )
  )