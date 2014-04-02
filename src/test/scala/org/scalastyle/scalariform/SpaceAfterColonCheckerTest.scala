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

import org.scalastyle.file.CheckerTest
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test

class SpaceAfterColonCheckerTest extends AssertionsForJUnit with CheckerTest {
  override protected val key: String = "space.after.colon"
  override protected val classUnderTest = classOf[SpaceAfterColonChecker]

  @Test def testOK(): Unit = {
    val source =
      """
        |package foobar
        |
        |class Dummy {
        |  val a: Int = 0
        |  def b: Int = 1
        |  var c: Int = 2
        |}
        |
      """.stripMargin

    assertErrors(List(), source)
  }


  @Test def testNewLine(): Unit = {
    val source =
      """
        |package foobar
        |
        |class Dummy {
        |  val a:
        |  Int = 0
        |  def b
        |  : Int = 1
        |  var
        |  c: Int = 2
        |}
        |
      """.stripMargin

    assertErrors(List(), source)
  }

  @Test def testEdgeCases(): Unit = {
    val source =
      """
        |package foobar
        |
        |class Dummy[T: Manifest] {
        |  val a: Int = 0
        |  // A:B:C
        |  def b(ch: Int): Int = 1
        |  d ++: e
        |  d :\ e
        |  e /: d
        |}
        |
      """.stripMargin

    assertErrors(List(), source)
  }

  @Test def testFailureCases(): Unit = {
    val source =
      """
        |package foobar
        |
        |class Dummy[T : Manifest] {
        |  val a : Int = 0
        |  // A:B:C
        |  def b (ch   : Int):    Int = 1
        |  def c (ch: Int) : Int = 1
        |  val d:Int = 2
        |  val e :Int = 3
        |}
        |
      """.stripMargin

    assertErrors(List(columnError(4, 14), columnError(5, 8), columnError(7, 14), columnError(7, 20),
      columnError(8, 18), columnError(9, 7), columnError(10, 8)), source)
  }
}
