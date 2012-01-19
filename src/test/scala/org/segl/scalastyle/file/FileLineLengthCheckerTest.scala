package org.segl.scalastyle.file

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.segl.scalastyle.Checker
import org.segl.scalastyle.StyleError
import org.segl.scalastyle.Message
import org.segl.scalastyle.ConfigCheck

import java.util.Set;

import org.junit.Before;
import org.junit.Test;

class FileLineLengthCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "line.size.limit"
  val classUnderTest = classOf[FileLineLengthChecker]

  @Test def testNoMax() = {
    val source = """
package foobar
import foobar
    object Foobar {
}
""";

    assertErrors(List(), source, Map("maxLineLength" -> "20"))
  }

  @Test def testWithOneMax() = {
    val source = """
package foobar
import foobar
    object Foobar {
}
""";

    assertErrors(List(lineError(4, List("15"))), source, Map("maxLineLength" -> "15"))
  }

  @Test def testWithTwoMax() = {
    val source = """
package foobar
import foobar
    object Foobar {
}
    object Barbar {
}
""";

    assertErrors(List(lineError(4, List("15")), lineError(6, List("15"))), source, Map("maxLineLength" -> "15"))
  }

  @Test def testWithSpacesTabs() = {
    val source = """
package foobar

import# #foo
object Barbar {
}
""".replaceAll("#","\t");
    println(source)

    assertErrors(List(lineError(4, List("14")), lineError(5, List("14"))), source, Map("maxLineLength" -> "14"))
  }
}
