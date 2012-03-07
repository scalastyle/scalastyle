// Copyright (C) 2011-2012 the original author or authors.
// See the LICENCE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package org.segl.scalastyle.file

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.segl.scalastyle.Checker
import org.segl.scalastyle.StyleError
import org.segl.scalastyle.Message
import org.segl.scalastyle.WarningLevel
import org.segl.scalastyle.ConfigurationChecker
import org.segl.scalastyle.FileSpec

trait CheckerTest {
  protected val key: String
  protected val classUnderTest: Class[_ <: Checker[_]]

  object NullFileSpec extends FileSpec {
    def name() = ""
  }

  protected def assertErrors[T <: FileSpec](list: List[Message[T]], source: String, params: Map[String, String] = Map()) = {
    assertEquals(list, Checker.verifySource(List(ConfigurationChecker(classUnderTest.getName(), WarningLevel, params)), NullFileSpec, source))
  }

  protected def fileError(args: List[String] = List()) = StyleError(NullFileSpec, classUnderTest, key, WarningLevel, args, None, None)
  protected def lineError(line: Int, args: List[String] = List()) = StyleError(NullFileSpec, classUnderTest, key, WarningLevel, args, Some(line), None)
  protected def columnError(line: Int, column: Int, args: List[String] = List()) =
                StyleError(NullFileSpec, classUnderTest, key, WarningLevel, args, Some(line), Some(column))
//  protected def positionError(position: Int) = StyleError(null, key, Some(position), Some(position))
}

