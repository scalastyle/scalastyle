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

class ParameterNumberCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "parameterNumber"
  val classUnderTest = classOf[ParameterNumberChecker]

  @Test def testOK() = {
    val source = """
package foobar

class OK {
  def method(i1: Int, i2: Int, i3: Int, i4: Int, i5: Int, i6: Int, i7: Int, i8: Int): Int = 45
}
""";

    assertErrors(List(), source)
  }

  @Test def testKO() = {
    val source = """
package foobar

class OK {
  def method(i1: Int, i2: Int, i3: Int, i4: Int, i5: Int, i6: Int, i7: Int, i8: Int, i9: Int): Int = 45
}
""";

    assertErrors(List(columnError(5, 6)), source)
  }

  @Test def testOuterKOInnerKO() = {
    val source = """
package foobar

class Outer {
  object Inner {
    def method(i1: Int, i2: Int, i3: Int, i4: Int, i5: Int, i6: Int, i7: Int, i8: Int, i9: Int): Int = 45
  }

  class Inner {
    def method(i1: Int, i2: Int, i3: Int, i4: Int, i5: Int, i6: Int, i7: Int, i8: Int, i9: Int): Int = 45
  }
}    
""";

    assertErrors(List(columnError(6, 8), columnError(10, 8)), source)
  }
}
