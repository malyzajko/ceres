name := "ceres"

version := "0.0"

scalaVersion := "2.10.2"

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

//scalacOptions in ThisBuild += "-feature"

fork := true

javaOptions += "-Djava.library.path=lib/"

scalaSource in Compile <<= baseDirectory(_ / "src")
