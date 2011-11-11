package org.segl.scalastyle.file

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.segl.scalastyle.Checker
import org.segl.scalastyle.StyleError
import org.segl.scalastyle.Message
import org.segl.scalastyle.ConfigCheck

trait CheckerTest {
  def assertErrors[T <: Checker](list: List[Message], source: String, params: Map[String, String] = Map())(implicit manifest: Manifest[T]) = {
	assertEquals(list, Checker.verifySource(List(ConfigCheck(manifest.erasure.getName(), params)), null, source))
  }
}

