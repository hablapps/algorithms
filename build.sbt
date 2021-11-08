name := "algorithms"

version := "0.1"

scalaVersion := "2.13.6"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test

libraryDependencies += "org.typelevel" %% "cats-core" % "2.3.0"

scalacOptions ++= Seq("-Xlog-implicits")