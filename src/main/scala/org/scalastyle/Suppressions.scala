package org.scalastyle

//todo private, comments
case class Suppression(fileRegex: String, rulesToExcludeRegex: String)

// todo move
case class FileNameAndRules[T <: FileSpec](fileSpec: T, scalastyleConfig: ScalastyleConfiguration)
