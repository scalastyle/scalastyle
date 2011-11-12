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

class NullCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "null"
  val classUnderTest = classOf[NullChecker]

	@Test def testZero() = {
	  val source = """
package foobar
	    
object Foobar {
  val foo = 1
}
""";
	  
	  assertErrors(List(), source)
	}
	
	@Test def testOne() = {
	  val source = """
package foobar

object Foobar {
  val foo: String = null
  val bar: String = null
}
""";

	  assertErrors(List(positionError(57), positionError(83)), source)
	}
}
