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

import org.junit.Test
import org.junit.Assert._

class SuppressionParserTest {

  @Test
  def testParse_success: Unit = {
    val fileContents =
      "<suppressions>\n" +
      "  <suppress files=\"somepath/.*.scala\" checks=\".*\"/>\n" +
      "</suppressions>"

    val suppressionsEither = SuppressionParser.parseString(fileContents)

    assertTrue(suppressionsEither.isRight)

    val suppressions = suppressionsEither.right.toOption.get
    assertEquals(1, suppressions.size)
    assertEquals("somepath/.*.scala", suppressions(0).fileRegex)
    assertEquals(".*", suppressions(0).rulesToExcludeRegex)
  }

  @Test
  def testParse_badXmlFile: Unit = {
    val fileContents =
      "<wrongTag>\n" +
        "  <suppress files=\"somepath/.*.scala\" checks=\".*\"/>\n" +
      "</wrongTag>"

    val suppressionsEither = SuppressionParser.parseString(fileContents)

    assertTrue(suppressionsEither.isLeft)

    val suppressions = suppressionsEither.left.toOption.get
    assertEquals("Top level xml attribute was wrongTag. Expected 'suppressions'", suppressions.moreInfo)
  }

  @Test
  def testSuppressionMatchesSomeFileWildCard: Unit = {
    val suppression = Suppression(".*", null)

    assertTrue(SuppressionParser.suppMatchesFile(suppression, "/Users/mbileschi/somename"))
  }

  @Test
  def testSuppressionMatchesSomeFilePartialWildCard: Unit = {
    val suppression = Suppression(".*somename.*", null)

    assertTrue(SuppressionParser.suppMatchesFile(suppression, "/Users/mbileschi/somename"))
  }

  @Test
  def testSuppressionMatchesSomeFileNoWildCard: Unit = {
    val suppression = Suppression("/Users/mbileschi/somename", null)

    assertTrue(SuppressionParser.suppMatchesFile(suppression, "/Users/mbileschi/somename"))
  }

  @Test
  def testRulesForFile_fileMatches_ruleMatches: Unit = {
    val suppressions =  Seq(Suppression(".*", ".*"))
    val configuration = scalastyleConfigurationForRulesWithClassNames("rule1")
    val rulesForFile = SuppressionParser.checksForSuppressions("fileName", configuration, suppressions)

    assertTrue(rulesForFile.checks.isEmpty)
  }

  @Test
  def testRulesForFile_fileMatches_ruleDoesNotMatch: Unit = {
    val suppressions =  Seq(Suppression(".*", "notmatching"))
    val configuration = scalastyleConfigurationForRulesWithClassNames("rule1")
    val rulesForFile = SuppressionParser.checksForSuppressions("fileName", configuration, suppressions)

    assertEquals(1, rulesForFile.checks.size)
    assertEquals(configuration, rulesForFile)
  }

  @Test
  def testRulesForFile_fileDoesNotMatch_ruleMatches: Unit = {
    val suppressions =  Seq(Suppression("notmatching", ".*"))
    val configuration = scalastyleConfigurationForRulesWithClassNames("rule1")
    val rulesForFile = SuppressionParser.checksForSuppressions("fileName", configuration, suppressions)

    assertEquals(1, rulesForFile.checks.size)
    assertEquals(configuration, rulesForFile)
  }

  @Test
  def testRulesForFile_twoRules_bothMatch: Unit = {
    val suppressions =  Seq(Suppression(".*", "rule1"), Suppression(".*", "rule2"))
    val configuration = scalastyleConfigurationForRulesWithClassNames("rule1", "rule2")
    val rulesForFile = SuppressionParser.checksForSuppressions("fileName", configuration, suppressions)

    assertTrue(rulesForFile.checks.isEmpty)
  }

  @Test
  def testRulesForFile_twoRules_oneMatches: Unit = {
    val suppressions =  Seq(Suppression("nonMatching", "rule1"), Suppression(".*", "rule2"))
    val configuration = scalastyleConfigurationForRulesWithClassNames("rule1", "rule2")
    val rulesForFile = SuppressionParser.checksForSuppressions("fileName", configuration, suppressions)

    assertEquals(1, rulesForFile.checks.size)
    assertEquals(configuration.copy(checks = List(configurationCheckerFor("rule1"))), rulesForFile)
  }

  @Test
  def testFilesAndRulesAfterSuppressionsNoneMatch: Unit = {
    val suppressions =  Seq(Suppression(".*", "rule1"), Suppression(".*", "rule2"))
    val configuration = scalastyleConfigurationForRulesWithClassNames("rule1", "rule2")
    val actual = SuppressionParser.filesAndRulesAfterSuppressions(Seq(new SourceSpec("name", "contents")), configuration, suppressions)
    val expected = Seq()

    assertEquals(expected, actual)
  }

  private[this] def scalastyleConfigurationForRulesWithClassNames(names: String*) = {
    val rules = names.map(configurationCheckerFor).toList
    ScalastyleConfiguration("test", false, rules)
  }

  private[this] def configurationCheckerFor(name: String) = {
    ConfigurationChecker(name, ErrorLevel, false, Map(), None, None)
  }
}

