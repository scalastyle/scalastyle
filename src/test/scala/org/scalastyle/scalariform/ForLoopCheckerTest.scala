package org.scalastyle.scalariform

import org.junit.Test
import org.scalastyle.file.CheckerTest
import org.scalatestplus.junit.AssertionsForJUnit

// scalastyle:off magic.number

class ForLoopCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "for.loop"
  val classUnderTest = classOf[ForLoopChecker]

  @Test def testOK(): Unit = {
    val source =
      """
package foobar

class Foobar {
  for ( t <- List(1,2,3)) {
    println(t)
  }
}
"""

    assertErrors(List(), source)
  }

  @Test def testCurlyBracesUsed(): Unit = {
    val source =
      """
package foobar

class Foobar {
   for {
     p <- List(1,2,3)
     t <- List(2,4,6)
    } {
      println(p + t)
    }
 }
"""

    assertErrors(List(columnError(5, 7)), source)
  }

  @Test def testIgnoresComprehension(): Unit = {
    val source =
      """
package foobar

class Foobar {
   for {
     p <- List(1,2,3)
     t <- List(2,4,6)
    } yield(p + t)
 }
"""

    assertErrors(List(), source)
  }
}
