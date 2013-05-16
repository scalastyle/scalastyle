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

package org.scalastyle.file

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.scalastyle.Checker
import org.scalastyle.StyleError
import org.scalastyle.Message
import org.scalastyle.WarningLevel
import org.scalastyle.ConfigurationChecker
import org.scalastyle.FileSpec
import org.scalastyle.ScalastyleConfiguration

// scalastyle:off multiple.string.literals

trait CheckerTest {
  protected val key: String
  protected val classUnderTest: Class[_ <: Checker[_]]

  object NullFileSpec extends FileSpec {
    def name(): String = ""
  }

  protected def assertErrors[T <: FileSpec](expected: List[Message[T]], source: String, params: Map[String, String] = Map(),
                                            customMessage: Option[String] = None, commentFilter: Boolean = true, customId: Option[String] = None) = {
    val classes =  List(ConfigurationChecker(classUnderTest.getName(), WarningLevel, true, params, customMessage, customId))
    val configuration = ScalastyleConfiguration("", commentFilter, classes)
    assertEquals(expected.mkString("\n"), Checker.verifySource(configuration, classes, NullFileSpec, source).mkString("\n"))
  }

  protected def fileError(args: List[String] = List(), customMessage: Option[String] = None) =
          StyleError(NullFileSpec, classUnderTest, key, WarningLevel, args, None, None, customMessage)
  protected def lineError(line: Int, args: List[String] = List()) = StyleError(NullFileSpec, classUnderTest, key, WarningLevel, args, Some(line), None)
  protected def columnError(line: Int, column: Int, args: List[String] = List()) =
                StyleError(NullFileSpec, classUnderTest, key, WarningLevel, args, Some(line), Some(column))
}

