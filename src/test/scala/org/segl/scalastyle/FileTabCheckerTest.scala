package org.segl.scalastyle

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert.assertEquals;
import org.junit.Assert.assertTrue;

import java.util.Set;

import org.junit.Before;
import org.junit.Test;

trait CheckerTest {
  def assertErrors[T <: Checker](list: List[Message], source: String)(implicit manifest: Manifest[T]) = {
	assertEquals(list, Checker.verifySource(List(ConfigCheck(manifest.erasure.getName(), Map())), null, source))
  }
}

class FileTabCheckerTest extends AssertionsForJUnit with CheckerTest {
	@Test def testZero() = {
	  val source = """
package foobar
import foobar
    object Foobar {
}
""";
	  
	  assertErrors[FileTabChecker](List(), source)
	}

	@Test def testOne() = {
	  val source = """
package foobar
import foobar
	object Foobar {
}
""";
	  
	  assertErrors[FileTabChecker](List(StyleError(null, "line.contains.tab", Some(4), Some(0), None)), source)
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
	  
	  assertErrors[FileTabChecker](List(StyleError(null, "line.contains.tab", Some(4), Some(0), None), StyleError(null, "line.contains.tab", Some(6), Some(0), None)), source)
	}
}	   