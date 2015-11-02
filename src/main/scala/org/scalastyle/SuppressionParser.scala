package org.scalastyle

import scala.io.Source
import scala.xml.{Node, NodeSeq}

/**
 * Provides support for working with checkstyle-like suppressions files.
 *
 * Example:
 * <suppressions>
 *   <suppress files="regex1" checks="regex2"/>
 * </suppressions>
 *
 * This allows us to exclude checks containing regex2 from being run on files
 * containing regex1.
 */
object SuppressionParser {
  case class MalformedXML(moreInfo: String)

  /** Namespace for parsing suppressions XML file. */
  object Xml {
    val TopLevelLabel = "suppressions"
    val InnerLabel = "suppress"
    val SuppressFileAttr = "files"
    val ChecksAttr = "checks"
  }

  def parseFile(fileName: String): Either[MalformedXML, Seq[Suppression]] = {
    parseString(Source.fromFile(fileName).mkString)
  }

  //todo
  // Visible for testing.
  def parseString(fileContents: String): Either[MalformedXML, Seq[Suppression]] = {
    val xml = scala.xml.XML.loadString(fileContents)
    if (xml.label != Xml.TopLevelLabel) {
      Left(MalformedXML("Top level xml attribute was " + xml.label + ". Expected '" + Xml.TopLevelLabel + "'"))
    } else {
      val suppressionsNodeSeq = xml \ Xml.InnerLabel
      val suppressions = suppressionsNodeSeq.map { (node) =>
        val fileRegex = node.attribute(Xml.SuppressFileAttr).head.text
        val rulesRegex = node.attribute(Xml.ChecksAttr).head.text
        Suppression(fileRegex, rulesRegex)
      }
      Right(suppressions)
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
      checks = checks.filterNot(checker => suppMatchesCheck(suppression, checker))
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
