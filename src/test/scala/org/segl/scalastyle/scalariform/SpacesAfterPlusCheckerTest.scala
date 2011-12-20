package org.segl.scalastyle.scalariform

import org.segl.scalastyle.file.CheckerTest
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.segl.scalastyle.Checker
import org.segl.scalastyle.StyleError
import java.util.Set
import org.junit.Before
import org.junit.Test

class SpacesAfterPlusCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "spaces.after.plus"
  val classUnderTest = classOf[SpacesAfterPlusChecker]

  @Test def testOK() = {
    val source = """
package foobar

object Foobar {
  val foo = 1 + 2
}
""";

    assertErrors(List(), source)
  }

  @Test def testNoSpaces() = {
    val source = """
package foobar

object Foobar {
  val foo = 1 +2
}
""";

    assertErrors(List(columnError(5, 14)), source)
  }

  @Test def testTwoSpaces() = {
    val source = """
package foobar

object Foobar {
  val foo = 1 +  2
}
""";

    assertErrors(List(), source)
  }
}
