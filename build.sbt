import AssemblyKeys._

name := "scalastyle"

organization := "org.scalastyle"

scalaVersion := "2.10.4"

scalacOptions ++= Seq("-deprecation")

crossScalaVersions := Seq("2.10.4", "2.11.2")

description := "Scalastyle style checker for Scala"

libraryDependencies ++= Seq(
                        "com.danieltrinh" %% "scalariform" % "0.1.5",
                        "com.typesafe" % "config" % "1.2.0",
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

buildInfoKeys := Seq[BuildInfoKey](organization, name, version, scalaVersion, sbtVersion)

buildInfoPackage := "org.scalastyle"

seq(filterSettings: _*)

aetherPublishBothSettings

aether.Aether.aetherLocalRepo := Path.userHome / "dev" / "repo"

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Managed

releaseSettings

ReleaseKeys.crossBuild := true

val dynamicPublish = Def.taskDyn {
  if (version.value.trim.endsWith("SNAPSHOT")) {
    Def.task { publish.value }
  } else {
    Def.task { PgpKeys.publishSigned.value }
  }
}

ReleaseKeys.publishArtifactsAction := dynamicPublish.value
