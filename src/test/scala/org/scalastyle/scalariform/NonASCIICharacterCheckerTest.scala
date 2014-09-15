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

import org.junit.Test
import org.scalastyle.file.CheckerTest
import org.scalatest.junit.AssertionsForJUnit

// scalastyle:off magic.number

class NonASCIICharacterCheckerTest extends AssertionsForJUnit with CheckerTest {
  override protected val key: String = "non.ascii.character.disallowed"
  override protected val classUnderTest = classOf[NonASCIICharacterChecker]


  @Test def testClassOK(): Unit = {
    val source = """
                   |package foobar
                   |// ~~$%
                   |class OK {
                   |  def -> = "something"
                   |  val `=>` = "test"
                   |}""".stripMargin

    assertErrors(List(), source)
  }


  @Test def testClassNotOk(): Unit = {
    val source = """
                   |package foobar
                   |// \u2190
                   |class NotOK {
                   |  def \u2192 = "something"
                   |  def `\u21d2` = "test"
                   |}
                   | """.stripMargin
    assertErrors(List(columnError(2, 14), columnError(5, 6), columnError(6, 6)), source)
  }
}
