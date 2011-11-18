package org.segl.scalastyle.file

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test

/**
 * Test that checks that return keyword should not be used
 *
 * @author Galder Zamarreño
 */
class ReturnCheckerTest extends AssertionsForJUnit with CheckerTest {

   protected val classUnderTest = classOf[ReturnChecker]

   protected val key = "line.contains.return"

   @Test def testZeroErrors {
      val source = """
         |package foobar
         |import foobar
         |object Foobar {
         |}
         """.stripMargin
      assertErrors(List(), source)
   }

   @Test def testOneError {
      val source = """
         |package foobar
         |import foobar
         |object Foobar {
         |   def boo: String = {
         |      return ""
         |   }
         |}
         """.stripMargin
      assertErrors(List(columnError(5, 6)), source)
   }

}