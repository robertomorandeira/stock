name := "stock"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
  jdbc,
  "com.typesafe.play" %% "play-slick" % "0.8.0",
  cache,
  ws
)
