package org.scalastyle

import scala.xml.{Node, NodeSeq}

/**
 * Created by mbileschi on 10/29/15.
 */
class SuppressionParser {


}

object SuppressionParser {
  def parse(fileContents: String): Seq[Suppression] = {
    val xml = scala.xml.XML.loadString(fileContents)

    // todo consider making better? or safer?
    // consider making more strict
//    val suppressionsNode = first(xml \ "suppressions") //todo extract
    val suppressions = xml \ "suppress"
    suppressions.map { (node) =>
      Suppression(node.attribute("files").head.text, node.attribute("checks").head.text)
    }
  }

  //consider making T more scoped
  def filesAndRulesAfterSuppressions[T](
    candidateFiles: Seq[FileSpec],
    configuration: ScalastyleConfiguration,
    suppressions: Seq[Suppression]
  ): Seq[FileNameAndRules] = {
    for {
      fileSpec <- candidateFiles
      fileName = fileSpec.name
    } yield {
      if (someSuppressionMatchesFile(suppressions.map(_.fileRegex), fileName)) {
        //this needs to take both the file name and the suppression, and this approach won't work where we check to see if the suppression matches some file. need to loop over files, suppressions, checks, in that order.
        val newConfig = rulesForFile(suppressions.map(_.rulesToExcludeRegex), configuration, fileSpec)
        FileNameAndRules(fileSpec, newConfig)
      } else {
        FileNameAndRules(fileSpec, configuration.copy())
      }
    }
  }

  def someSuppressionMatchesFile(
    fileSuppressionRegexes: Seq[String],
    fileName: String
  ): Boolean = {
    fileSuppressionRegexes.exists(_.r.findFirstIn(fileName).isDefined)
  }

  def rulesForFile(
    ruleSuppressionRegexes: Seq[String],
    configuration: ScalastyleConfiguration,
    fileSpec: FileSpec
  ): ScalastyleConfiguration = {
    var checks = configuration.checks
    for (suppression <- ruleSuppressionRegexes) {
      checks = checks.filterNot(check => suppressionMatchesCheck(suppression, check))
    }
    configuration.copy(checks = checks)
  }

  private[this] def suppressionMatchesCheck(suppressionRegex: String, rule: ConfigurationChecker) =
    suppressionRegex.r.findFirstIn(rule.className).isDefined

}
