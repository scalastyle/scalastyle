import AssemblyKeys._

name := "scalastyle"

organization := "org.scalastyle"

version := "0.6.0-SNAPSHOT"

scalaVersion := "2.10.4"

scalacOptions ++= Seq("-deprecation")

crossScalaVersions := Seq("2.11.1")

description := "Scalastyle style checker for Scala"

def scalariformDependency(scalaVersion: String) = scalaVersion match {
  case "2.11.1" => "com.danieltrinh" %% "scalariform" % "0.1.5"
  case _ => "org.scalariform" %% "scalariform" % "0.1.4"
}

libraryDependencies <+= scalaVersion(scalariformDependency(_))

libraryDependencies ++= Seq(
                        "com.typesafe" % "config" % "1.2.0",
                        "junit" % "junit" % "4.11" % "test",
                        "com.novocode" % "junit-interface" % "0.10" % "test",
                        "com.google.guava" % "guava" % "17.0" % "test",
                        "org.scalatest" %% "scalatest" % "2.2.2" % "test")

fork in Test := true

javaOptions in Test += "-Dfile.encoding=UTF-8"

publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (version.value.trim.endsWith("SNAPSHOT")) Some("snapshots" at nexus + "content/repositories/snapshots") 
  else Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

//seq(bintrayPublishSettings:_*)

//bintray.Keys.repository in bintray.Keys.bintray := "scalastyle"

//bintray.Keys.bintrayOrganization in bintray.Keys.bintray := Some("scalastyle")

licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0.html"))

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>http://www.scalastyle.org</url>
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
  art.copy(`classifier` = Some("batch"))
}

addArtifact(artifact in (Compile, assembly), assembly)

mainClass in assembly := Some("org.scalastyle.Main")

buildInfoSettings

sourceGenerators in Compile <+= buildInfo

buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion)

buildInfoPackage := "org.scalastyle"

seq(filterSettings: _*)

aetherSettings
