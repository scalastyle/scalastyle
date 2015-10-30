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
      if (someSuppressionMatchesFile(suppressions, fileName)) {
        rulesForFile(suppressions, configuration, fileSpec)
      } else {
        FileNameAndRules(fileSpec, configuration.copy())
      }
    }
  }

  def someSuppressionMatchesFile(
    suppressions: Seq[Suppression],
    fileName: String
  ): Boolean = {
    suppressions.exists(_.fileRegex.r.findFirstIn(fileName).isDefined)
  }

  //todo better scoping in this file
  //todo comments
  def rulesForFile(
    suppressions: Seq[Suppression],
    configuration: ScalastyleConfiguration,
    fileSpec: FileSpec
  ): FileNameAndRules = {
    var checks = configuration.checks
    for (suppression <- suppressions) {
      checks = checks.filterNot { check =>
        suppression.rulesToExcludeRegex.r.findFirstIn(check.className).isDefined
      }
    }
    FileNameAndRules(fileSpec, configuration.copy(checks = checks))
  }

  //todo remove?
  private[this] def first(nodeSeq: NodeSeq): Node = nodeSeq.iterator.toList(0)
}
