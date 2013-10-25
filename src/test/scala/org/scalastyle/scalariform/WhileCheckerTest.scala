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

class WhileCheckerTest extends AssertionsForJUnit with CheckerTest {

  protected val classUnderTest = classOf[WhileChecker]

  protected val key = "while"

  @Test def testZeroErrors(): Unit = {
    val source = """
class C1 {
  def m1(n: Int) = n
}
""";
    assertErrors(List(), source)
  }

  @Test def testOneError(): Unit = {
    val source = """
class C1 {
  def m1(n: Int) = {
    var count = 0
    while (count < n) {
      count += 1
    }
    count
  }
}
"""
    assertErrors(List(columnError(5, 4)), source)
  }

  @Test def testTwoErrors(): Unit = {
    val source = """
class C1 {
  def m1(n: Int) = {
    var count = 0
    while (count < n) {
      var count2 = count
      while (count2 < n) {
        count2 += 1
      }
      count += 1
    }
    count
  }
}
"""
    assertErrors(List(columnError(5, 4), columnError(7, 6)), source)
  }
}
