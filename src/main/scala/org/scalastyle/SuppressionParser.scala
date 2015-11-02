package org.scalastyle

import scala.io.Source
import scala.xml.{Node, NodeSeq}

/**
 * Created by mbileschi on 10/29/15.
 */
class SuppressionParser {


}

object SuppressionParser {
  def parseFile(fileName: String): Seq[Suppression] = {
    parseString(Source.fromFile(fileName).mkString)
  }

  //private?
  def parseString(fileContents: String): Seq[Suppression] = {
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
  def filesAndRulesAfterSuppressions[T <: FileSpec](
    candidateFiles: Seq[T],
    configuration: ScalastyleConfiguration, //consider decreasing surface area
    suppressions: Seq[Suppression]
  ): Seq[FileNameAndRules[T]] = {
    val filesAndRules = for {
      fileSpec <- candidateFiles
      fileName = fileSpec.name
    } yield {
      val scalastyleConfig = checksForSuppressions(fileName, configuration, suppressions)
      FileNameAndRules(fileSpec, scalastyleConfig)
    }

    filesAndRules.filter(_.scalastyleConfig.checks.nonEmpty)
  }

  //private
  def checksForSuppressions(
    fileName: String,
    configuration: ScalastyleConfiguration,
    suppressions: Seq[Suppression]
  ): ScalastyleConfiguration = {
    var checks = configuration.checks
    for {
      suppression <- suppressions
      if suppMatchesFile(suppression, fileName)
    } {
      println("supp matches file " + fileName)
      checks = checks.filterNot(checker => suppMatchesCheck(suppression, checker))
      println("checks: " + checks)
    }
    configuration.copy(checks = checks)
  }

  //private
  def suppMatchesFile(suppression: Suppression, fileName: String ): Boolean =
    suppression.fileRegex.r.findFirstIn(fileName).isDefined

  //private
  def suppMatchesCheck(suppression: Suppression, rule: ConfigurationChecker) =
    suppression.rulesToExcludeRegex.r.findFirstIn(rule.className).isDefined
}
