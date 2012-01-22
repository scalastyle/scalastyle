package org.segl.scalastyle.file

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.segl.scalastyle.Checker
import org.segl.scalastyle.StyleError
import org.segl.scalastyle.Message

import java.util.Set;

import org.junit.Before;
import org.junit.Test;

class FileTabCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "line.contains.tab"
  val classUnderTest = classOf[FileTabChecker]

  @Test def testZero() = {
    val source = """
package foobar
import foobar
    object Foobar {
}
""";

    assertErrors(List(), source)
  }

  @Test def testOne() = {
    val source = """
package foobar
import foobar
#object Foobar {
}
""".replaceAll("#", "\t");

    assertErrors(List(columnError(4, 0)), source)
  }

  @Test def testTwo() = {
    val source = """
package foobar
import foobar
#object Foobar {
}
#object Barbar {
}
""".replaceAll("#", "\t");

    assertErrors(List(columnError(4, 0), columnError(6, 0)), source)
  }
}
