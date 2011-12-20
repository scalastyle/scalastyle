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

class FileLengthCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "file.size.limit"
  val classUnderTest = classOf[FileLengthChecker]

  @Test def testZero() = {
    val source = """
package foobar
import foobar
  object Foobar {
}
""";

    assertErrors(List(), source, Map("maxFileLength" -> "5"))
  }

  @Test def testOne() = {
    val source = """
package foobar
import foobar
  object Foobar {
}
  object Barbar {
}
""";

    assertErrors(List(fileError()), source, Map("maxFileLength" -> "5"))
  }
}
