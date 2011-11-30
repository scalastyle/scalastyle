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

class MagicNumberCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "magic.number"
  val classUnderTest = classOf[MagicNumberChecker]

	@Test def testZero() = {
	  val source = """
package foobar
	    
class Foobar {
  val foo1 = -1
  val foo2 = 0
  val foo3 = 1
  val foo4 = 2
  val foo5 = 3
  val foo6 = 4
}
""";
	  
	  assertErrors(List(positionError(119), positionError(135)), source)
	}
}
