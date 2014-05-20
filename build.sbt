name := "ceres"

version := "0.0"

scalaVersion := "2.10.2"

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

//scalacOptions in ThisBuild += "-feature"

fork := true

javaOptions += "-Djava.library.path=lib/"

scalaSource in Compile <<= baseDirectory(_ / "src")

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "com.github.axel22" %% "scalameter" % "0.4"

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

parallelExecution in Test := false
