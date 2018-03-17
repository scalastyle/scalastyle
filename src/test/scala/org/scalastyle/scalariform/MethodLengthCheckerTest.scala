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

// scalastyle:off magic.number multiple.string.literals

class MethodLengthCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "method.length"
  val classUnderTest = classOf[MethodLengthChecker]

  @Test def testOK(): Unit = {
    val source = """
package foobar

class F1() {
  def method1() = {
    def foobar() = { 5 }
    2
    3
    4
    5
    6
    7
  }
  def method2() = {
    1
    2
    3
    4
    5
    6
  }
  def method3() = {
    def foobar() = { 5 }
    2
    3
    4
    5
    6
  }
}
"""

    assertErrors(List(), source, Map("maxLength" -> "7"))
  }

  @Test def testIgnoreComments(): Unit = {
    val source =
      """
package foobar

class F2() {
  def method1() = {
    1
    2
    /** (3)
     *  (4)
     */ (5)
    3   (6)
    4   (7)
    5   (8)
  }
  def method2() = {
    // (1)
    1  (2)
    2  (3)
    // (4)
    // (5)
    3  (6)
    4  (7)
    5  (8)
  }
}
"""

    assertErrors(List(), source, Map("maxLength" -> "5", "ignoreComments" -> "true"))
  }

  @Test def testIgnoreEmpty(): Unit = {
    val source =
      """
package foobar

class F2() {
  def method1() = {
    1  (1)
    2  (2)

    3  (4)
    4  (5)
    5  (6)
  }
  def method2() = {

    1 (2)
    2 (3)
    3 (4)
    4 (5)
    5 (6)

  }
}
"""

    assertErrors(List(), source, Map("maxLength" -> "5", "ignoreEmpty" -> "true"))
  }

  @Test def testIgnoreEmptyWithErrors(): Unit = {
    val source =
      """
package foobar

class F2() {
  def method1() = {
    1  (1)
    2  (2)

    3  (4)
    4  (5)
    5  (6)
  }
}
"""

    assertErrors(List(columnError(5, 6, List("4"))), source, Map("maxLength" -> "4", "ignoreEmpty" -> "true"))
  }

  @Test def testIgnoreEmptyAndComments(): Unit = {
    val source =
      """
package foobar

class F2() {
  def method1() = {
    1  (1)
    2  (2)

    3  (4)
    4  (5)
    5  (6)
    // (7)
  }
  def method2() = {

    1   (2)
    2   (3)
    //  (4)

    3   (6)
    4   (7)
    /** (8)
     *  (9)
     */ (10)

    5   (12)

  }
}
"""

    assertErrors(List(), source, Map("maxLength" -> "5", "ignoreComments" -> "true", "ignoreEmpty" -> "true"))
  }

  @Test def testNotIgnoreComments(): Unit = {
    val source =
      """
package foobar

class F2() {
  def method1() = {
    1
    2
    /** (3)
     *  (4)
     */ (5)
    3   (6)
  }
  def method2() = {
    // (1)
    1  (2)
    2  (3)
    // (4)
    // (5)
    3  (6)
  }
}
"""

    assertErrors(
      List(columnError(5, 6, List("5")), columnError(13, 6, List("5"))),
      source,
      Map("maxLength" -> "5", "ignoreComments" -> "false")
    )
  }

  @Test def testIgnoreCommentsComplicated(): Unit = {
    val source = """
class F3() {
  def method1() = {
    1 //
    (2) /*  */
    2
    3
    (4) /*
         *  (5)
         */ (6)
    4 (7)
    5 (8)
  }
  def method2() = {
    // /* (1)
    1     (2)
    2     (3)
    3     (5)
    4     (6)
    5     (7)
    6     (8)
  }
}
"""
    assertErrors(
      List(columnError(14, 6, List("5"))),
      source,
      Map("maxLength" -> "5", "ignoreComments" -> "true")
    )
  }
}
