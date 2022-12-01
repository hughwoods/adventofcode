name := "adventofcode"

version := "0.1"

lazy val twentyTwentyOne = Project("twentyTwentyOne", file("2021"))
  .settings(
    scalaVersion := "2.13.7",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.3.0",
      "org.typelevel" %% "cats-parse" % "0.3.6",
      "org.scalactic" %% "scalactic" % "3.2.10",
      "org.scalatest" %% "scalatest" % "3.2.10" % "test"
    )
  )
