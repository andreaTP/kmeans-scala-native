
name := "kmeans-scala-native"

organization := "eu.unicredit"

scalaVersion := "2.11.8"

sources in doc in Compile := List()

libraryDependencies ++= Seq(
  compilerPlugin("org.scala-native" %  "tools_2.10" % "0.1-SNAPSHOT"),
  compilerPlugin("org.scala-native" %  "nir_2.10"   % "0.1-SNAPSHOT"),
  compilerPlugin("org.scala-native" %  "util_2.10"  % "0.1-SNAPSHOT"),
                 "org.scala-native" %% "clib"       % "0.1-SNAPSHOT",
                 "org.scala-native" %% "javalib"    % "0.1-SNAPSHOT",
                 "org.scala-native" %% "scalalib"   % "0.1-SNAPSHOT"
  )

scala.scalanative.sbtplugin.ScalaNativePlugin.projectSettings

nativeVerbose := true

nativeClangOptions ++= Seq("-O3", "-lm")

//nativeEmitDependencyGraphPath := Some(file("out.dot"))
