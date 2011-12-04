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
  protected val classUnderTest: Class[_ <: Checker]
  
  protected def assertErrors[T <: FileSpec](list: List[Message[T]], source: String, params: Map[String, String] = Map()) = {
    val description = list.map( message => {
      message match {
        case StyleError(x, y, _, _, Some(pos)) => "(" + pos + "):" + substring(source, pos);
        case _ => "unknown"
      }
    }).mkString(",");
	assertEquals(description, list, Checker.verifySource(List(ConfigCheck(classUnderTest.getName(), params)), null, source))
  }
  
  private[this] def substring(s: String, pos: Int) = s.substring(pos, scala.math.min(s.length(), pos + 7))
  
  protected def positionError(position: Int) = StyleError(null, key, None, None, Some(position))
  protected def fileError() = StyleError(null, key, None, None, None)
  protected def lineError(line: Int) = StyleError(null, key, Some(line), None, None)
  protected def columnError(line: Int, column: Int) = StyleError(null, key, Some(line), Some(column), None)
}

