package org.segl.scalastyle.file

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.segl.scalastyle.Checker
import org.segl.scalastyle.StyleError
import org.segl.scalastyle.Message
import org.segl.scalastyle.ConfigCheck
import org.segl.scalastyle.FileSpec

trait CheckerTest {
  protected val key: String
  protected val classUnderTest: Class[_ <: Checker[_]]
  
  protected def assertErrors[T <: FileSpec](list: List[Message[T]], source: String, params: Map[String, String] = Map()) = {
	assertEquals(list, Checker.verifySource(List(ConfigCheck(classUnderTest.getName(), params)), null, source))
  }
  
  protected def fileError() = StyleError(null, key, None, None)
  protected def lineError(line: Int) = StyleError(null, key, Some(line), None)
  protected def columnError(line: Int, column: Int) = StyleError(null, key, Some(line), Some(column))
//  protected def positionError(position: Int) = StyleError(null, key, Some(position), Some(position))
}

