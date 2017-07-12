package org.scalastyle.scalariform

import org.junit.Test
import org.scalastyle.file.CheckerTest
import org.scalatest.junit.AssertionsForJUnit

class NamedArgumentCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "named.argument"
  val classUnderTest = classOf[NamedArgumentChecker]

  @Test def testDefault(): Unit = {
    val source = """b(b = true)
                   |i(i = 1)
                   |l(l = 1L)
                   |f(f = 1.2)
                   |c(c = 'a')
                   |n(n = null)
                 """.stripMargin
    val badSource = """b(true)
                      |i(1)
                      |l(1L)
                      |f(1.2)
                      |c('a')
                      |n(null)
                    """.stripMargin

    assertErrors(List.empty, source)
    assertErrors(List(
      columnError(1, 2),
      columnError(2, 2),
      columnError(3, 2),
      columnError(4, 2),
      columnError(5, 2),
      columnError(6, 2)), badSource)
  }

  @Test def testString(): Unit = {
    val source = """s1(s = "abc")
                   |s2(s = s"a$bc")
                 """.stripMargin
    val badSource = """s1("abc")
                      |s2(s"a${b}c")
                    """.stripMargin

    assertErrors(List.empty, source, Map("checkString" -> "true"))
    assertErrors(List(
      columnError(1, 3),
      columnError(2, 3)), badSource, Map("checkString" -> "true"))
  }

  @Test def testIgnore(): Unit = {
    val source1 = """setF(1)"""
    val source2 = """f(1)"""

    assertErrors(List.empty, source1)
    assertErrors(List(columnError(1, 2)), source2)

    assertErrors(List(columnError(1, 5)), source1, Map("ignoreMethod" -> "^f$"))
    assertErrors(List.empty, source2, Map("ignoreMethod" -> "^f$"))
  }

}
