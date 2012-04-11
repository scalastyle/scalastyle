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

class RegexCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "regex"
  val classUnderTest = classOf[RegexChecker]

  private val source = """//
package foobar


class foobar {

  def aMethod: String = {
    println("SHOULD NOT BE HERE")
    "TEST STRING";
  }

}
"""

  // scalastyle:off magic.number
  private val LineOne = 1
  private val LineThree = 3
  private val LineFive = 5
  private val LineSeven = 7
  private val LineNine = 9
  private val LineTen = 10
  private val LineTwelve = 12
  private val ColumnZero: Int = 0
  private val ColumnTwo: Int = 2
  private val ColumnSeventeen: Int = 17
  // scalastyle:on magic.number

  @Test
  def testSimpleCheck() {
    assertErrors(List(columnError(LineSeven, ColumnTwo, List("def"))), source,
      Map("expression" -> "def"))
  }

  @Test
  def testNoSemiColon() {
    assertErrors(List(columnError(LineNine, ColumnSeventeen, List(";"))), source,
      Map("expression" -> ";"))
  }

  @Test
  def testStartOfLineIsZeroColumn() {
    assertErrors(List(columnError(LineFive, ColumnZero, List("class"))), source,
      Map("expression" -> "class"))
  }

  @Test
  def testCanMatchLastCharInFile() {
    assertErrors(List(columnError(LineTwelve, ColumnZero, List("(?m)^}$"))), source, Map("expression" -> "(?m)^}$"))
  }

  @Test
  def testCanMatchFirstCharInFile() {
    assertErrors(List(columnError(LineOne, ColumnZero, List("(?m)^//$"))), source, Map("expression" -> "(?m)^//$"))
  }

  @Test
  def testNoDoubleBlankLines() {
    assertErrors(List(columnError(LineThree, ColumnZero, List("(?m)^\\s*$(\\r|)\\n^\\s*$(\\r|)\\n"))), source,
      Map("expression" -> "(?m)^\\s*$(\\r|)\\n^\\s*$(\\r|)\\n"))
  }

  @Test
  def testMultipleMatchesReportMultipleErrors() {
    assertErrors(List(columnError(LineTen, ColumnTwo, List("}")), columnError(LineTwelve, ColumnZero, List("}"))), source,
      Map("expression" -> "}"))
  }

  @Test
  def testCannotFindMatch() {
    assertErrors(List(), source, Map("expression" -> "^SHOULD$"))
  }

  @Test
  def testSingleMatchWithBoundsCheck() {
    assertErrors(List(columnError(LineTwelve, ColumnZero, List("(?m)^}$"))), source, Map("expression" -> "(?m)^}$"))
  }
}
