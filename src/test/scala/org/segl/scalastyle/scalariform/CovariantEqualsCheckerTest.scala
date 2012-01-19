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

class CovariantEqualsCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "covariant.equals"
  val classUnderTest = classOf[CovariantEqualsChecker]

  @Test def testClassOK() = {
    val source = """
package foobar

class OK {
  def equals(o: java.lang.Object): Boolean = false
  def equals(o: java.lang.Integer): Boolean = false
}
""";

    assertErrors(List(), source)
  }

  @Test def testClassCovariantEqualsNoObjectKO() = {
    val source = """
package foobar

class CovariantEqualsNoObjectKO {
  def equals(o: java.lang.Integer): Boolean = false
}
""";

    assertErrors(List(columnError(4, 6)), source)
  }

  @Test def testObjectOK() = {
    val source = """
package foobar

object OK {
  def equals(o: java.lang.Object): Boolean = false
  def equals(o: java.lang.Integer): Boolean = false
}
""";

    assertErrors(List(), source)
  }

  @Test def testObjectCovariantEqualsNoObjectKO() = {
    val source = """
package foobar

object CovariantEqualsNoObjectKO {
  def equals(o: java.lang.Integer): Boolean = false
}
""";

    assertErrors(List(columnError(4, 7)), source)
  }
}
