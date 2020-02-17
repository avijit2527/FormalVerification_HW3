name := "z3-assignment"
version := "1.0"
scalaVersion := "2.11.6"
sbtVersion := "1.2.8"

javaOptions += s"-Djava.library.path=./lib"
javaOptions += "-Dtest=forked"
fork := true

// libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % "test"

// libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1" withSources()

