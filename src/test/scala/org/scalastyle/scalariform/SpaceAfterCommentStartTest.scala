package org.scalastyle.scalariform

import org.junit.Test
import org.scalastyle.Checker
import org.scalastyle.file.CheckerTest
import org.scalatest.junit.AssertionsForJUnit

class SpaceAfterCommentStartTest extends AssertionsForJUnit with CheckerTest {
  override protected val key: String = "space.after.comment.start"
  override protected val classUnderTest: Class[_ <: Checker[_]] = classOf[SpaceAfterCommentStart]

  @Test def testSinglelineComments(): Unit = {
    val source = """
package foobar

object Foobar {
  //Incorrect
  // correct comment
}"""

    assertErrors(List(columnError(5, 2)), source)
  }

  @Test def testMultipleInlineComments(): Unit = {
    val source = """
package foobar

object Foobar {
  //Incorrect
  // correct comment//not wrong//check
  val a = 10//Incorrect
  val b = 100 //Incorrect
  val c = 1// Correct
  val d = 2 // Correct
  val e = 3
}"""
      assertErrors(List(columnError(5, 2), columnError(7, 12), columnError(8, 14)), source)

  }

  @Test def testMultilineComments(): Unit = {
    val source = """
package foobar

object Foobar {
  /*WRONG
  *
  */
  /* Correct*/

  val d = 2 /*Wrong*/
  /*
   *Correct
   */
  val e = 3/* Correct*/
}"""

    assertErrors(List(columnError(5, 2), columnError(10, 12)), source)
  }


  @Test def testScaladocsComments(): Unit = {
    val source = """
package foobar

object Foobar {
  /**WRONG
  *
  */
  /** Correct*/
  val d = 2 /**Wrong*/
  /**
   *Correct
   */
  val e = 3/** Correct*/
}"""
    assertErrors(List(columnError(5, 2), columnError(9, 12)), source)
  }

  @Test def testMixedComments(): Unit = {
    val source = """
package foobar

object Foobar {
  /**WRONG
  *
  */
  /** Correct*/
  val d = 2 /*Wrong*/ //Wrong
  /**
   *Correct
   */
  val e = 3/** Correct*/ // Correct
}"""
    assertErrors(List(columnError(5, 2), columnError(9, 12), columnError(9, 22)), source)
  }
}