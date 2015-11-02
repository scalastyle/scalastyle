package org.scalastyle

import org.junit.Test
import org.junit.Assert._

import scala.util.Either.RightProjection

/**
 * Created by mbileschi on 10/30/15.
 */
class SuppressionParserTest {
  @Test
  def testParse_success: Unit = {
    //todo: worry about relative paths
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
    //todo: worry about relative paths
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
    val rule = ConfigurationChecker("className", ErrorLevel, false, Map(), None, None)
    val configuration = ScalastyleConfiguration("test", false, List(rule))
    val rulesForFile = SuppressionParser.checksForSuppressions("fileName", configuration, suppressions)

    assertTrue(rulesForFile.checks.isEmpty)
  }

  @Test
  def testRulesForFile_fileMatches_ruleDoesNotMatch: Unit = {
    val suppressions =  Seq(Suppression(".*", "notmatching"))
    val rule = ConfigurationChecker("className", ErrorLevel, false, Map(), None, None)
    val configuration = ScalastyleConfiguration("test", false, List(rule))
    val rulesForFile = SuppressionParser.checksForSuppressions("fileName", configuration, suppressions)

    assertEquals(1, rulesForFile.checks.size)
    assertEquals(configuration, rulesForFile)
  }

  @Test
  def testRulesForFile_fileDoesNotMatch_ruleMatches: Unit = {
    val suppressions =  Seq(Suppression("notmatching", ".*"))
    val rule = ConfigurationChecker("className", ErrorLevel, false, Map(), None, None)
    val configuration = ScalastyleConfiguration("test", false, List(rule))
    val rulesForFile = SuppressionParser.checksForSuppressions("fileName", configuration, suppressions)

    assertEquals(1, rulesForFile.checks.size)
    assertEquals(configuration, rulesForFile)
  }

  @Test
  def testRulesForFile_twoRules_bothMatch: Unit = {
    val suppressions =  Seq(Suppression(".*", "rule1"), Suppression(".*", "rule2"))
    val rule1 = ConfigurationChecker("rule1", ErrorLevel, false, Map(), None, None)
    val rule2 = ConfigurationChecker("rule2", ErrorLevel, false, Map(), None, None)
    val configuration = ScalastyleConfiguration("test", false, List(rule1, rule2))
    val rulesForFile = SuppressionParser.checksForSuppressions("fileName", configuration, suppressions)

    assertTrue(rulesForFile.checks.isEmpty)
  }

  @Test
  def testRulesForFile_twoRules_oneMatches: Unit = {
    val suppressions =  Seq(Suppression("nonMatching", "rule1"), Suppression(".*", "rule2"))
    val rule1 = ConfigurationChecker("rule1", ErrorLevel, false, Map(), None, None)
    val rule2 = ConfigurationChecker("rule2", ErrorLevel, false, Map(), None, None)
    val configuration = ScalastyleConfiguration("test", false, List(rule1, rule2))
    val rulesForFile = SuppressionParser.checksForSuppressions("fileName", configuration, suppressions)

    assertEquals(1, rulesForFile.checks.size)
    assertEquals(configuration.copy(checks = List(rule1)), rulesForFile)
  }

  @Test
  def testFilesAndRulesAfterSuppressionsNoneMatch: Unit = {
    val suppressions =  Seq(Suppression(".*", "rule1"), Suppression(".*", "rule2"))
    val rule1 = ConfigurationChecker("rule1", ErrorLevel, false, Map(), None, None)
    val rule2 = ConfigurationChecker("rule2", ErrorLevel, false, Map(), None, None)
    val configuration = ScalastyleConfiguration("test", false, List(rule1, rule2))
    val actual = SuppressionParser.filesAndRulesAfterSuppressions(Seq(new SourceSpec("name", "contents")), configuration, suppressions)
    val expected = Seq()

    assertEquals(expected, actual)
  }
}

