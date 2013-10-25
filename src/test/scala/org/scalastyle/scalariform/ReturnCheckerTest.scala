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

package org.scalastyle.scalariform

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import org.scalastyle.file.CheckerTest

// scalastyle:off magic.number

/**
 * Test that checks that return keyword should not be used
 *
 * @author Galder Zamarreño
 */
class ReturnCheckerTest extends AssertionsForJUnit with CheckerTest {

  protected val classUnderTest = classOf[ReturnChecker]

  protected val key = "return"

  @Test def testZeroErrors(): Unit = {
    val source = """
         |package foobar
         |object Foobar {
         |}
         """.stripMargin
    assertErrors(List(), source)
  }

  @Test def testOneError(): Unit = {
    val source = """
         |package foobar
         |object Foobar {
         |   def boo: String = {
         |      return " return here"
         |   }
         |}
         """.stripMargin
    assertErrors(List(columnError(5, 6)), source)
  }
}
