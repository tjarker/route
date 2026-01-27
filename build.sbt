
scalaVersion := "2.13.14"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-language:reflectiveCalls",
)

fork := true
javaOptions += "--add-exports=java.base/jdk.internal.vm=ALL-UNNAMED"
javacOptions += "--add-exports=java.base/jdk.internal.vm=ALL-UNNAMED"

val chiselVersion = "3.6.1"
addCompilerPlugin("edu.berkeley.cs" %% "chisel3-plugin" % chiselVersion cross CrossVersion.full)
libraryDependencies += "edu.berkeley.cs" %% "chisel3" % chiselVersion

libraryDependencies += "io.github.tjarker" %% "liftoff" % "0.0.1" % "test"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"