resolvers += "jgit-repo" at "http://download.eclipse.org/jgit/maven"

//addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "0.6.2")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.9")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.7.0")

addSbtPlugin("com.github.sdb" % "xsbt-filter" % "0.4")

addSbtPlugin("com.typesafe.sbt" % "sbt-pgp" % "0.8.3")

addSbtPlugin("no.arktekk.sbt" % "aether-deploy" % "0.13")

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "5.2.4")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.11")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.0")
