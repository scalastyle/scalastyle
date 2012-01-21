package org.segl.scalastyle.file

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.segl.scalastyle.Checker
import org.segl.scalastyle.StyleError
import org.segl.scalastyle.Message
import org.segl.scalastyle.WarningLevel
import org.segl.scalastyle.ConfigCheck
import org.segl.scalastyle.FileSpec

trait CheckerTest {
  protected val key: String
  protected val classUnderTest: Class[_ <: Checker[_]]

  protected def assertErrors[T <: FileSpec](list: List[Message[T]], source: String, params: Map[String, String] = Map()) = {
    assertEquals(list, Checker.verifySource(List(ConfigCheck(classUnderTest.getName(), WarningLevel, params)), null, source))
  }

  protected def fileError(args: List[String] = List()) = StyleError(null, classUnderTest, key, WarningLevel, args, None, None)
  protected def lineError(line: Int, args: List[String] = List()) = StyleError(null, classUnderTest, key, WarningLevel, args, Some(line), None)
  protected def columnError(line: Int, column: Int, args: List[String] = List()) = StyleError(null, classUnderTest, key, WarningLevel, args, Some(line), Some(column))
//  protected def positionError(position: Int) = StyleError(null, key, Some(position), Some(position))
}

