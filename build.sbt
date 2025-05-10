import sbt._

organization := "org.suecarter"

name := "tablediff"

scalaVersion := "3.7.1"
crossScalaVersions := Seq(scalaVersion.value, "2.13.16")

version := "1.1.1"

Global / onChangedBuildSource := ReloadOnSourceChanges

libraryDependencies ++= Seq(
  "org.apache.commons" % "commons-lang3" % "3.17.0",
  "org.scalatest" %% "scalatest" % "3.2.19" % "test",
  "com.novocode" % "junit-interface" % "0.11" % Test,
)

// Want to keep testing the SampleApp and this ups the Java integration coverage
Test / unmanagedSourceDirectories += baseDirectory.value / "SampleApp/src/test/java"

Compile / scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-deprecation",
  "-feature",
)

fork := true

licenses := Seq(
  "MIT" -> url("http://opensource.org/licenses/MIT")
)

homepage := Some(url("http://github.com/smootoo/TableDiff"))

publishTo := localStaging.value

sonatypeProfileName := organization.value

versionScheme := Some("early-semver")

sonatypeProjectHosting := Some(xerial.sbt.Sonatype.GitHubHosting("smootoo", "TableDiff", "squishback@gmail.com"))

developers := List(
  Developer(id="smootoo", name="Sue Carter", email="squishback@gmail.com", url=url("https://suecarter.org"))
)

scmInfo := Some(
  ScmInfo(
    url("https://github.com/smootoo/TableDiff"),
    "scm:git:git@github.com:smootoo/TableDiff.git"
  )
)
