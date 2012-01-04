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

class NoFinalizeCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "no.finalize"
  val classUnderTest = classOf[NoFinalizeChecker]

  @Test def testClassOK() = {
    val source = """
package foobar

class OK {
  def finalize(o: java.lang.Integer): Unit = {}
}
""";

    assertErrors(List(), source)
  }

  @Test def testClassCovariantEqualsNoObjectKO() = {
    val source = """
package foobar

class CloneKO {
  def finalize(): Unit = {}
}
""";

    assertErrors(List(columnError(4, 6)), source)
  }

  @Test def testObjectOK() = {
    val source = """
package foobar

object OK {
  def finalize(o: java.lang.Integer): Unit = {}
}
""";

    assertErrors(List(), source)
  }

  @Test def testObjectCovariantEqualsNoObjectKO() = {
    val source = """
package foobar

object CloneKO {
  def finalize(): Unit = {}
}
""";

    assertErrors(List(columnError(4, 7)), source)
  }
}
