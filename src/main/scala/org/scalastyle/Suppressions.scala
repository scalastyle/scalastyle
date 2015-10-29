package org.scalastyle

class Suppressions(val get: Seq[Suppression]) {

}

//todo private, comments
case class Suppression(fileRegex: String, rulesToExcludeRegex: String)
