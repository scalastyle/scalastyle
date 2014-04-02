package org.scalastyle.scalariform

import org.junit.Test
import org.scalastyle.file.CheckerTest
import org.scalatest.junit.AssertionsForJUnit

class SingleSpaceAfterKeywordTest extends AssertionsForJUnit with CheckerTest {
  override protected val key: String = "single.space.after.keyword"
  override protected val classUnderTest = classOf[SingleSpaceAfterKeyword]

  @Test def testIf(): Unit = {
    val source = """
package foobar

object Foobar {
  if(true) false
  if (true) false
  if  (true) false

}"""

    assertErrors(List(columnError(5, 4), columnError(7, 6)), source)
  }

  @Test def testClass(): Unit = {
    val source = """
package foobar

object Foobar {
case class A[T, X]() extends(T, X)
case class A[T, X]() extends (T, X)
}"""

    assertErrors(List(columnError(5, 28)), source)
  }
}
