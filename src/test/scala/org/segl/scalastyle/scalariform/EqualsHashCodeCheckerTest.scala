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

class EqualsHashCodeCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "equalsHashCode"
  val classUnderTest = classOf[EqualsHashCodeChecker]

  @Test def testOK() = {
    val source = """
package foobar

class OK {
  def hashCode(): Int = 45
  def equals(o: java.lang.Object): Boolean = false
}
""";

    assertErrors(List(), source)
  }

  @Test def testHashCodeOnlyKO() = {
    val source = """
package foobar

class HashCodeOnlyKO {
  def hashCode(): Int = 45
}
""";

    assertErrors(List(columnError(4, 6)), source)
  }

  @Test def testEqualsOnlyKO() = {
    val source = """
package foobar

class EqualsOnlyKO {
  def equals(o: java.lang.Object): Boolean = false
}
""";

    assertErrors(List(columnError(4, 6)), source)
  }

  @Test def testEqualsOnlyAnyKO() = {
    val source = """
package foobar

class EqualsOnlyKO {
  def equals(o: Any): Boolean = false
}
""";

    assertErrors(List(columnError(4, 6)), source)
  }

  @Test def testEqualsWrongSignatureOK() = {
    val source = """
package foobar

class EqualsWrongSignatureOK {
  def equals(o: scala.Object): Boolean = false
  def equals(o: java.lang.Object)(o2: java.lang.Object): Boolean = false
}
""";

    assertErrors(List(), source)
  }

  @Test def testHashCodeWrongSignatureOK() = {
    val source = """
package foobar

class HashCodeWrongSignatureOK {
  def hashCode(o: scala.Object): Int = 45
}
""";

    assertErrors(List(), source)
  }

  @Test def testOuterKOInnerKO() = {
    val source = """
package foobar

class OuterKO {
  def hashCode(): Int = 45
  class InnerKO {
    def equals(o: java.lang.Object): Boolean = false
  }
}
""";

    assertErrors(List(columnError(4, 6), columnError(6, 8)), source)
  }

  @Test def testOuterOKInnerKO() = {
    val source = """
package foobar

class OuterOK {
  def hashCode(): Int = 45
  class InnerKO {
    def equals(o: java.lang.Object): Boolean = false
  }
  def equals(o: java.lang.Object): Boolean = false
}
""";

    assertErrors(List(columnError(6, 8)), source)
  }

  @Test def testObjectInnerKO() = {
    val source = """
package foobar

object Object {
  class ObjectInnerKO {
    def equals(o: java.lang.Object): Boolean = false
  }
}
""";

    assertErrors(List(columnError(5, 8)), source)
  }

  @Test def testMultipleClasses() = {
    val source = """
package foobar

class Class1 {
  def hashCode(): Int = 45
}

class Class2 {
  def hashCode(): Int = 45
}
""";

    assertErrors(List(columnError(4, 6), columnError(8, 6)), source)
  }
}
