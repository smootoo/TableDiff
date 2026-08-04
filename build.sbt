import sbt._

organization := "org.suecarter"

name := "tablediff"

scalaVersion := "3.8.4"

version := "1.1.3"

Global / onChangedBuildSource := ReloadOnSourceChanges

libraryDependencies ++= Seq(
  "org.apache.commons" % "commons-lang3" % "3.20.0",
  "org.scalatest" %% "scalatest" % "3.2.20" % "test",
  "com.novocode" % "junit-interface" % "0.11" % Test,
)

// Want to keep testing the SampleApp and this ups the Java integration coverage
Test / unmanagedSourceDirectories += baseDirectory.value / "SampleApp/src/test/java"

Compile / scalacOptions ++= Seq(
  "-Werror",
  "-deprecation",
  "-feature",
)

fork := true


// Publishing
licenses := Seq(
  License.MIT
)

homepage := Some(uri("https://github.com/smootoo/TableDiff"))

// https://www.scala-sbt.org/2.x/docs/en/recipes/central.html?highlight=sonat#step-2-credentials
publishTo := {
  val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
  if version.value.endsWith("-SNAPSHOT") then Some("central-snapshots" at centralSnapshots)
  else localStaging.value
}

versionScheme := Some("early-semver")

developers := List(
  Developer(id="smootoo", name="Sue Carter", email="squishback@gmail.com", url=uri("https://suecarter.org"))
)

scmInfo := Some(
  ScmInfo(
    uri("https://github.com/smootoo/TableDiff"),
    "scm:git:git@github.com:smootoo/TableDiff.git"
  )
)
