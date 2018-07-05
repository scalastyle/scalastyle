import AssemblyKeys._

name := "scalastyle"

organization := "org.scalastyle"

scalaVersion := "2.10.7"

scalacOptions ++= Seq("-deprecation", "-feature")

crossScalaVersions := Seq("2.10.7", "2.11.12", "2.12.6")

description := "Scalastyle style checker for Scala"

libraryDependencies ++= Seq(
                        "org.scalariform" %% "scalariform" % "0.2.0",
                        "com.typesafe" % "config" % "1.2.0",
                        "junit" % "junit" % "4.11" % "test",
                        "com.novocode" % "junit-interface" % "0.10" % "test",
                        "com.google.guava" % "guava" % "17.0" % "test",
                        "org.scalatest" %% "scalatest" % "3.0.3" % "test")

fork in (Test, run) := true

javaOptions in Test += "-Dfile.encoding=UTF-8"

coverageHighlighting := scalaBinaryVersion.value != "2.10"

publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (version.value.trim.endsWith("SNAPSHOT")) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0.html"))

pomIncludeRepository := { _ => false }

pomExtra := <url>http://www.scalastyle.org</url>
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
  </developers>

assemblySettings

artifact in (Compile, assembly) ~= { art =>
  art.copy(`classifier` = Some("batch"))
}

addArtifact(artifact in (Compile, assembly), assembly)

mainClass in assembly := Some("org.scalastyle.Main")
mainClass in (Compile, run) := Some("org.scalastyle.Main")

buildInfoSettings

sourceGenerators in Compile += buildInfo

buildInfoKeys := Seq[BuildInfoKey](organization, name, version, scalaVersion, sbtVersion)

buildInfoPackage := "org.scalastyle"

filterSettings

if (System.getProperty("scalastyle.publish-ivy-only") == "true") {
  Seq()
}  else {
  Seq(aetherPublishBothSettings: _*)
}

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

val createRulesMarkdown = taskKey[Unit]("deploy to a server")

val createRulesMarkdownDyn = Def.taskDyn {
  val t = (target.value / "rules-dev.markdown").getAbsolutePath
  Def.task {
    (runMain in Compile).toTask(" org.scalastyle.util.CreateRulesMarkdown " + t).value
  }
}

createRulesMarkdown := createRulesMarkdownDyn.value
