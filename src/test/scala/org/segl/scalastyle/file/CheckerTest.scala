package org.segl.scalastyle.file

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.segl.scalastyle.Checker
import org.segl.scalastyle.StyleError
import org.segl.scalastyle.Message
import org.segl.scalastyle.ConfigCheck

trait CheckerTest {
  protected val key: String
  
  protected def assertErrors[T <: Checker](list: List[Message], source: String, params: Map[String, String] = Map())(implicit manifest: Manifest[T]) = {
	assertEquals(list, Checker.verifySource(List(ConfigCheck(manifest.erasure.getName(), params)), null, source))
  }
  
  protected def positionError(position: Int) = StyleError(null, key, None, None, Some(position))
  protected def fileError() = StyleError(null, key, None, None, None)
  protected def lineError(line: Int) = StyleError(null, key, Some(line), None, None)
  protected def columnError(line: Int, column: Int) = StyleError(null, key, Some(line), Some(column), None)
}

