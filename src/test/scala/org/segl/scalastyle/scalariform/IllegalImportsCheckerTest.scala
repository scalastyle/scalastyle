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

class IllegalImportsCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "illegal.imports"
  val classUnderTest = classOf[IllegalImportsChecker]

  @Test def testZero() = {
    val source = """
package foobar

import java.util._

object Foobar {
  val foo = 1
}
""";

    assertErrors(List(), source)
  }

  @Test def testOne() = {
    val source = """|package foobar
                      |
                      |import java.util._
                      |import sun.com.foobar;
                      |import sun._
                      |
                      |object Foobar {
                      |}
""".stripMargin;

    assertErrors(List(columnError(4, 0), columnError(5, 0)), source)
  }
}
