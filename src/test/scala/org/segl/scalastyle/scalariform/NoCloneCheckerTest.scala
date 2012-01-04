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

class NoCloneCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "no.clone"
  val classUnderTest = classOf[NoCloneChecker]

  @Test def testClassOK() = {
    val source = """
package foobar

class OK {
  def clone(o: java.lang.Integer): Any = null
}
""";

    assertErrors(List(), source)
  }

  @Test def testClassCovariantEqualsNoObjectKO() = {
    val source = """
package foobar

class CloneKO {
  def clone(): Any = null
}
""";

    assertErrors(List(columnError(4, 6)), source)
  }

  @Test def testObjectOK() = {
    val source = """
package foobar

object OK {
  def clone(o: java.lang.Integer): Any = null
}
""";

    assertErrors(List(), source)
  }

  @Test def testObjectCovariantEqualsNoObjectKO() = {
    val source = """
package foobar

object CloneKO {
  def clone(): Any = null
}
""";

    assertErrors(List(columnError(4, 7)), source)
  }
}
