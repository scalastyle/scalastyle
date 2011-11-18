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

class ClassNamesCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "class.name"
  val classUnderTest = classOf[ClassNamesChecker]

	@Test def testZero() = {
	  val source = """
package foobar
	    
class Foobar {
  val foo = 1
}
""";
	  
	  assertErrors(List(), source)
	}
	
	@Test def testOne() = {
	  val source = """
package foobar

class foobar {
  class barbar {
  }
}
""";

	  assertErrors(List(positionError(20), positionError(38)), source)
	}
}

class ObjectNamesCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "object.name"
  val classUnderTest = classOf[ObjectNamesChecker]

	@Test def testZero() = {
	  val source = """
package foobar
	    
class Foobar {
  val foo = 1
}
""";
	  
	  assertErrors(List(), source)
	}
	
	@Test def testOne() = {
	  val source = """
package foobar

object foobar {
  object barbar {
  }
}
""";

	  assertErrors(List(positionError(20), positionError(39)), source)
	}
}
