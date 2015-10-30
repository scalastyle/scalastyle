package org.scalastyle

import org.junit.Test
import org.junit.Assert.assertTrue
import org.junit.Assert.assertEquals

/**
 * Created by mbileschi on 10/30/15.
 */
class SuppressionParserTest {
  @Test
  def testParse: Unit = {
    //todo: worry about relative paths
    val fileContents =
      "<suppressions>\n" +
      "  <suppress files=\"somepath/.*.scala\" checks=\".*\"/>\n" +
      "</suppressions>"

    val suppressions = SuppressionParser.parse(fileContents)

    assertEquals(1, suppressions.size)
    assertEquals("somepath/.*.scala", suppressions(0).fileRegex)
    assertEquals(".*", suppressions(0).rulesToExcludeRegex)
  }
  // todo test failure modes? make prettier errors?

  @Test
  def testSuppressionMatchesSomeFileWildCard: Unit = {
    val suppressions = List(Suppression(".*", ".*"))

    assertTrue(SuppressionParser.someSuppressionMatchesFile(suppressions, "/Users/mbileschi/somename"))
  }

  @Test
  def testSuppressionMatchesSomeFilePartialWildCard: Unit = {
    val suppressions = List(Suppression(".*somename.*", ".*"))

    assertTrue(SuppressionParser.someSuppressionMatchesFile(suppressions, "/Users/mbileschi/somename"))
  }

  @Test
  def testSuppressionMatchesSomeFileNoWildCard: Unit = {
    val suppressions = List(Suppression("/Users/mbileschi/somename" , ".*"))

    assertTrue(SuppressionParser.someSuppressionMatchesFile(suppressions, "/Users/mbileschi/somename"))
  }

  @Test
  def testRulesForFile: Unit = {
    val suppressions =  List(Suppression(".*", ".*"))
    val rule = ConfigurationChecker("className", ErrorLevel, false, Map(), None, None)
    val configuration = ScalastyleConfiguration("test", false, List(rule))
    val fileSpec = new SourceSpec("name", "contents")
    val rulesForFile = SuppressionParser.rulesForFile(suppressions, configuration, fileSpec)

    assertEquals(fileSpec, rulesForFile.fileSpec) //todo consider refactoring so we don't return an unmutated value
    assertTrue(rulesForFile.scalastyleConfig.checks.isEmpty)
  }
}

