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
	@Test def testZero() = {
	  val source = """
package foobar
import foobar
	object Foobar {
}
""";
	  
	  assertErrors[WhitespaceEndOfLineChecker](List(), source)
	}

	@Test def testOne() = {
	  val source = """
package foobar  
import foobar
	object Foobar {
}
""";
	  
	  assertErrors[WhitespaceEndOfLineChecker](List(StyleError(null, "whitespace.end.of.line", Some(2), Some(14))), source)
	}

	@Test def testTwo() = {
	  val source = """
package foobar  
import foobar	
	object Foobar {
}
""";
	  
	  assertErrors[WhitespaceEndOfLineChecker](List(StyleError(null, "whitespace.end.of.line", Some(2), Some(14)), StyleError(null, "whitespace.end.of.line", Some(3), Some(13))), source)
	}
}
