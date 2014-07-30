import AssemblyKeys._

name := "scalastyle"

organization := "org.scalastyle"

versionWithGit

git.baseVersion := "0.5.0"

scalaVersion := "2.10.4"

scalacOptions ++= Seq("-deprecation")

crossScalaVersions := Seq("2.9.3")

description := "Scalastyle style checker for Scala"

def scalatestDependency(scalaVersion: String) = scalaVersion match {
  case "2.9.3" => "org.scalatest" % "scalatest_2.9.3" % "2.0.M5b" % "test"
  case _ => "org.scalatest" % "scalatest_2.10" % "2.0.M6-SNAP9" % "test"
}

libraryDependencies ++= Seq(
                        "org.scalariform" %% "scalariform" % "0.1.4",
                        "com.typesafe" % "config" % "1.2.0",
                        "junit" % "junit" % "4.11" % "test",
                        "com.novocode" % "junit-interface" % "0.10" % "test",
                        "org.reflections" % "reflections" % "0.9.9-RC2" % "test" exclude("org.javassist", "javassist") exclude("com.google.guava", "guava"),
                        "com.google.guava" % "guava" % "15.0" % "test",
                        "org.javassist" % "javassist" % "3.18.2-GA" % "test")

libraryDependencies <+= scalaVersion(scalatestDependency(_))

fork in Test := true

publishMavenStyle := true

seq(bintrayPublishSettings:_*)

bintray.Keys.repository in bintray.Keys.bintray := "scalastyle"

bintray.Keys.bintrayOrganization in bintray.Keys.bintray := Some("scalastyle")

licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0.html"))

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>http://www.scalastyle.org</url>
  <licenses>
    <license>
      <name>Apache 2.0</name>
      <url>http://www.apache.org/licenses/LICENSE-2.0.html</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>scm:git:git@github.com:scalastyle/scalastyle.git</url>
    <connection>scm:git:git@github.com:scalastyle/scalastyle.git</connection>
  </scm>
  <developers>
    <developer>
      <id>matthewfarwell</id>
      <name>Matthew Farwell</name>
      <url>http://www.farwell.co.uk</url>
    </developer>
  </developers>)

assemblySettings

artifact in (Compile, assembly) ~= { art =>
  art.copy(`classifier` = Some("assembly"))
}

addArtifact(artifact in (Compile, assembly), assembly)

mainClass in assembly := Some("org.scalastyle.Main")

buildInfoSettings

sourceGenerators in Compile <+= buildInfo

buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion)

buildInfoPackage := "org.scalastyle"
