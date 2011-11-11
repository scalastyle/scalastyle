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

class FileLineLengthCheckerTest extends AssertionsForJUnit with CheckerTest {
	@Test def testZero() = {
	  val source = """
package foobar
import foobar
	object Foobar {
}
""";
	  
	  assertErrors[FileLineLengthChecker](List(), source, Map("maxLineLength" -> "20"))
	}

	@Test def testOne() = {
	  val source = """
package foobar
import foobar
	object Foobar {
}
""";
	  
	  assertErrors[FileLineLengthChecker](List(StyleError(null, "line.size.limit", Some(4))), source, Map("maxLineLength" -> "15"))
	}

	@Test def testTwo() = {
	  val source = """
package foobar
import foobar
	object Foobar {
}
	object Barbar {
}
""";
	  
	  assertErrors[FileLineLengthChecker](List(StyleError(null, "line.size.limit", Some(4)), StyleError(null, "line.size.limit", Some(6))), source, Map("maxLineLength" -> "15"))
	}
}
