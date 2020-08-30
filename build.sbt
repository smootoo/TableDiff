import sbt._

organization := "org.suecarter"

name := "tablediff"

crossScalaVersions := Seq("2.13.3", "2.12.12")

version := "1.0.3"

//resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq(
  "org.apache.commons" % "commons-lang3" % "3.11",
  "org.scalatest" %% "scalatest" % "3.2.2" % "test",
  "com.novocode" % "junit-interface" % "0.11" % Test,
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
)

// Want to keep testing the SampleApp and this ups the Java integration coverage
unmanagedSourceDirectories in Test += baseDirectory.value / "SampleApp/src/test/java"

scalacOptions in Compile ++= Seq(
//  "-Xfatal-warnings", // @unchecked seems to be ignored
)

maxErrors := 1

fork := true

//javaOptions ++= Seq("-XX:MaxPermSize=256m", "-Xmx2g", "-XX:+UseConcMarkSweepGC")

coverageMinimum := 90
coverageFailOnMinimum := true
coverageHighlighting := false

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
