val dottyVersion = "0.20.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "parsing",
    version := "0.1.0",

    scalaVersion := dottyVersion,
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-unchecked",
    ),

    libraryDependencies ++= Seq(
      "ch.epfl.lamp" %% "dotty-staging" % scalaVersion.value,
      "com.novocode" % "junit-interface" % "0.11" % "test",
    )
  )
