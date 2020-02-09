import sbt._
import sbt.Keys.streams

enablePlugins(BuildInfoPlugin)

name := "scalastyle"
organization := "com.beautiful-scala"
description := "Scalastyle style checker for Scala"
homepage := Some(url("https://github.com/beautiful-scala/scalastyle"))
licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0.html"))
scmInfo := Some(
  ScmInfo(
    url("https://github.com/beautiful-scala/scalastyle"),
    "scm:git:https://github.com/beautiful-scala/scalastyle.git",
    Some("scm:git:git@github.com:beautiful-scala/scalastyle.git")
  )
)
developers := List(
  Developer(
    "mwz",
    "Michael Wizner",
    "@mwz",
    url("https://github.com/mwz")
  ),
  Developer(
    "matthewfarwell",
    "Matthew Farwell",
    "@matthewfarwell",
    url("http://www.farwell.co.uk")
  )
)

// Compile options
scalaVersion := "2.13.1"
crossScalaVersions := Seq("2.11.12", "2.12.10", "2.13.1")
scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-encoding",
  "utf8",
  "-feature",
  "-language:reflectiveCalls",
  "-Yrangepos",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Ywarn-unused",
  "-P:semanticdb:synthetics:on"
).filter {
  case ("-Yno-adapted-args") if scalaVersion.value.startsWith("2.13") =>
    false
  case _ =>
    true
}
javacOptions := Seq("-Xlint:deprecation")
javaOptions in Test += "-Dfile.encoding=UTF-8"
cancelable in Global := true

// Lib dependencies
libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-collection-compat" % "2.1.3",
  "org.scalariform"        %% "scalariform"             % "0.2.10",
  "com.typesafe"           % "config"                   % "1.2.1",
  "junit"                  % "junit"                    % "4.13" % "test",
  "com.novocode"           % "junit-interface"          % "0.11" % "test",
  "com.google.guava"       % "guava"                    % "23.0" % "test",
  "org.scalatest"          %% "scalatest"               % "3.0.8" % "test"
)

// Test
fork in (Test, run) := true
logBuffered in Test := false

// ScalaTest reporter config:
// -o - standard output,
// D - show all durations,
// T - show reminder of failed and cancelled tests with short stack traces,
// F - show full stack traces.
testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oTFNCO")

// scalafix & scalafmt
scalafixDependencies in ThisBuild ++= Seq(
  "org.scala-lang.modules" %% "scala-collection-migrations" % "2.1.3",
  "com.nequissimus"        %% "sort-imports"                % "0.3.1"
)
addCommandAlias("fix", "all compile:scalafix test:scalafix; fixImports")
addCommandAlias("fixImports", "compile:scalafix SortImports; test:scalafix SortImports")
addCommandAlias("fixCheck", "compile:scalafix --check; test:scalafix --check; fixCheckImports")
addCommandAlias("fixCheckImports", "compile:scalafix --check SortImports; test:scalafix --check SortImports")
scalafmtOnCompile in ThisBuild :=
  sys.env
    .get("CI")
    .forall(_.toLowerCase == "false")

// assembly
test in assembly := {}
artifact in (Compile, assembly) := {
  val art = (artifact in (Compile, assembly)).value
  art.withClassifier(Some("assembly"))
}
addArtifact(artifact in (Compile, assembly), assembly)
mainClass in assembly := Some("org.scalastyle.Main")
mainClass in (Compile, run) := Some("org.scalastyle.Main")

// build info
buildInfoKeys := Seq[BuildInfoKey](organization, name, version, scalaVersion, sbtVersion)
buildInfoPackage := "org.scalastyle"

// create rules
val createRulesMarkdown = taskKey[Unit]("deploy to a server")
val createRulesMarkdownDyn = Def.taskDyn {
  val t = (target.value / "rules-dev.markdown").getAbsolutePath
  Def.task {
    (runMain in Compile).toTask(" org.scalastyle.util.CreateRulesMarkdown " + t).value
  }
}
createRulesMarkdown := createRulesMarkdownDyn.value

// plugins
addCompilerPlugin(scalafixSemanticdb)
