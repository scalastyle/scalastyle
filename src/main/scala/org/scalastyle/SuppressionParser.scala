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
    val suppressionsNode = first(xml \ "suppressions") //todo extract
    val suppressions = suppressionsNode \ "suppress"
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

  private[this] def someSuppressionMatchesFile(
    suppressions: Seq[Suppression],
    fileName: String
  ): Boolean = {
    suppressions.exists(_.fileRegex.r.findFirstIn(fileName).isDefined)
  }

  private[this] def rulesForFile(
    suppressions: Seq[Suppression],
    configuration: ScalastyleConfiguration,
    fileSpec: FileSpec
  ): FileNameAndRules = {
    var checks = configuration.checks
    for (suppression <- suppressions) {
      checks = checks.filter { check =>
        suppression.rulesToExcludeRegex.r.findFirstIn(check.className).isDefined
      }
    }
    FileNameAndRules(fileSpec, configuration.copy(checks = checks))
  }

  private[this] def first(nodeSeq: NodeSeq): Node = nodeSeq.iterator.toList(0)
}
