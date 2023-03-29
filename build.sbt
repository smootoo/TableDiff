import sbt._

organization := "org.suecarter"

name := "tablediff"

crossScalaVersions := Seq("3.2.2", "2.13.10")
scalaVersion := "3.2.2"

version := "1.1"

Global / onChangedBuildSource := ReloadOnSourceChanges
//resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq(
  "org.apache.commons" % "commons-lang3" % "3.12.0",
  "org.scalatest" %% "scalatest" % "3.2.15" % "test",
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

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

credentials += Credentials(
  "Sonatype Nexus Repository Manager",
  "oss.sonatype.org",
  sys.env.getOrElse("SONATYPE_USERNAME", ""),
  sys.env.getOrElse("SONATYPE_PASSWORD", "")
)

pomExtra :=
  <scm>
  <url>git@github.com:smootoo/TableDiff.git</url>
  <connection>scm:git:git@github.com:smootoo/TableDiff.git</connection>
</scm>
<developers>
   <developer>
      <id>smootoo</id>
      <name>Sue Carter</name>
   </developer>
</developers>
