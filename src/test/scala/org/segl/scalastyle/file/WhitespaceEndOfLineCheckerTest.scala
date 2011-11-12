package org.segl.scalastyle.file

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.segl.scalastyle.Checker
import org.segl.scalastyle.StyleError
import org.segl.scalastyle.Message
import org.segl.scalastyle.ConfigCheck

import java.util.Set;

import org.junit.Before;
import org.junit.Test;

class WhitespaceEndOfLineCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "whitespace.end.of.line"
  val classUnderTest = classOf[WhitespaceEndOfLineChecker]
    
	@Test def testZero() = {
	  val source = """
package foobar
import foobar
	object Foobar {
}
""";
	  
	  assertErrors(List(), source)
	}

	@Test def testOne() = {
	  val source = """
package foobar  
import foobar
	object Foobar {
}
""";
	  
	  assertErrors(List(columnError(2, 14)), source)
	}

	@Test def testTwo() = {
	  val source = """
package foobar  
import foobar	
	object Foobar {
}
""";
	  
	  assertErrors(List(columnError(2, 14), columnError(3, 13)), source)
	}
}
