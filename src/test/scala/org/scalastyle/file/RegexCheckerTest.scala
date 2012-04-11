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

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

// scalastyle:off magic.number

class RegexCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "regex"
  val classUnderTest = classOf[RegexChecker]

  private val source = """//
package foobar


class foobar {

  def aMethod: String = {
    ("SHOULD NOT BE HERE")
    "TEST STRING";
  }

}
"""

  @Test
  def testSimpleCheck() {
    assertErrors(List(columnError(7, 2, List("def"))), source,
      Map("regex" -> "def"))
  }

  @Test
  def testNoSemiColon() {
    assertErrors(List(columnError(9, 17, List(";"))), source,
      Map("regex" -> ";"))
  }

  @Test
  def testStartOfLineIsZeroColumn() {
    assertErrors(List(columnError(5, 0, List("class"))), source,
      Map("regex" -> "class"))
  }

  @Test
  def testCanMatchLastCharInFile() {
    assertErrors(List(columnError(12, 0, List("(?m)^}$"))), source, Map("regex" -> "(?m)^}$"))
  }

  @Test
  def testCanMatchFirstCharInFile() {
    assertErrors(List(columnError(1, 0, List("(?m)^//$"))), source, Map("regex" -> "(?m)^//$"))
  }

  @Test
  def testNoDoubleBlankLines() {
    assertErrors(List(columnError(3, 0, List("(?m)^\\s*$(\\r|)\\n^\\s*$(\\r|)\\n"))), source,
      Map("regex" -> "(?m)^\\s*$(\\r|)\\n^\\s*$(\\r|)\\n"))
  }

  @Test
  def testMultipleMatchesReportMultipleErrors() {
    assertErrors(List(columnError(10, 2, List("}")), columnError(12, 0, List("}"))), source,
      Map("regex" -> "}"))
  }

  @Test
  def testCannotFindMatch() {
    assertErrors(List(), source, Map("regex" -> "^SHOULD$"))
  }

  @Test
  def testSingleMatchWithBoundsCheck() {
    assertErrors(List(columnError(12, 0, List("(?m)^}$"))), source, Map("regex" -> "(?m)^}$"))
  }
}
