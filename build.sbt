organization := "oddco"

name := "metaphone"

version := "1.0.0"

scalaVersion := "2.12.1"

mainClass in assembly := Option("oddco.Metaphone")

assemblyJarName in assembly := s"${name.value}.jar"
