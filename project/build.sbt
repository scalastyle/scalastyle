val scalastyleVersion = settingKey[String]("Scalastyle version")
scalastyleVersion := sys.props.getOrElse("scalastyle.version", "1.1.1")

libraryDependencies += "com.beautiful-scala" %% "scalastyle" % scalastyleVersion.value
