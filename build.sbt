name := "sangria-astdiff"

organization := "markarasev"

description := "Utility to compare sangria ast documents."

version := "0.1"

scalaVersion := "2.12.6"

libraryDependencies ++= Seq(
  "org.sangria-graphql" %% "sangria" % "1.4.1",
  "org.scalactic" %% "scalactic" % "3.0.5",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
)

publishTo := Some("Artifactory Realm" at "http://virgo.fun:8081/artifactory/sbt-repo-local")

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

enablePlugins(JavaAppPackaging)
