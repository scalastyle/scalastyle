// Copyright (C) 2011-2012 the original author or authors.
// See the LICENCE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package org.scalastyle

import scala.io.Source

/**
 * Models information about which checks we <i>shouldn't</i> run, and for which files those rules
 * shouldn't be run.
 */
case class Suppression(fileRegex: String, rulesToExcludeRegex: String)

/**
 * Models information about which rules we <i>should</i> run for a particular file, in addition
 * to how we should run those checks.
 */
case class FileNameAndRules[T <: FileSpec](fileSpec: T, scalastyleConfig: ScalastyleConfiguration)

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

  /**
   * Returns (roughly) a list of pairs (file, checks to run on that file).
   *
   * For each file, looks through the given suppressions, and if this file matches some suppression,
   * rules matching that suppression are filtered, and will not be run.
   */
  def filesAndRulesAfterSuppressions[T <: FileSpec](
    files: Seq[T],
    configuration: ScalastyleConfiguration,
    suppressions: Seq[Suppression]
  ): Seq[FileNameAndRules[T]] = {
    val filesAndRules = for {
      fileSpec <- files
      fileName = fileSpec.name
    } yield {
      val scalastyleConfig = checksForSuppressions(fileName, configuration, suppressions)
      FileNameAndRules(fileSpec, scalastyleConfig)
    }

    filesAndRules.filter(_.scalastyleConfig.checks.nonEmpty)
  }

  /**
   * Returns (roughly) a pairs (file, checks to run on that file).
   *
   * Looks through the given suppressions, and if this file matches some suppression,
   * rules matching that suppression are filtered, and will not be run.
   */
  // Visible for testing.
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

  // Visible for testing.
  def suppMatchesFile(suppression: Suppression, fileName: String ): Boolean =
    suppression.fileRegex.r.findFirstIn(fileName).isDefined

  // Visible for testing.
  def suppMatchesCheck(suppression: Suppression, rule: ConfigurationChecker) =
    suppression.rulesToExcludeRegex.r.findFirstIn(rule.className).isDefined
}
