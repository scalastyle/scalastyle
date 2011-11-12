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

class NoWhitespaceBeforeLeftBracketCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "no.whitespace.before.left.bracket"
  val classUnderTest = classOf[NoWhitespaceBeforeLeftBracketChecker]

	@Test def testOK() = {
	  val source = """
package foobar
	    
class Foobar[T] {
}
""";
	  
	  assertErrors(List(), source)
	}
	
	@Test def testOneSpace() = {
	  val source = """
package foobar

class Foobar [T] {
}
""";
	  
	  assertErrors(List(positionError(26)), source)
	}
	
	@Test def testTwoSpaces() = {
	  val source = """
package foobar

class Foobar [ Barbar [T]] {
}
""";
	  
	  assertErrors(List(positionError(26), positionError(35)), source)
	}
}

class NoWhitespaceAfterLeftBracketCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "no.whitespace.after.left.bracket"
  val classUnderTest = classOf[NoWhitespaceAfterLeftBracketChecker]

	@Test def testOK() = {
	  val source = """
package foobar
	    
class Foobar[T] {
}
""";
	  
	  assertErrors(List(), source)
	}
	
	@Test def testOneSpace() = {
	  val source = """
package foobar

class Foobar[ T] {
}
""";
	  
	  assertErrors(List(positionError(32)), source)
	}
	
	@Test def testTwoSpaces() = {
	  val source = """
package foobar

class Foobar[ Barbar[ T]] {
}
""";
	  
	  assertErrors(List(positionError(32), positionError(40)), source)
	}
}
