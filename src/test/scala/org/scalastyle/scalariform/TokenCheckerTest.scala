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

// scalastyle:off magic.number multiple.string.literals

class TokenCheckerTest extends AssertionsForJUnit with CheckerTest {
  protected val classUnderTest = classOf[TokenChecker]
  protected val key = "token"

  @Test def testErrors1(): Unit = {
    val source = """
object foo {
  def bar(x: Any) = x.asInstanceOf[Int]
}"""

    assertErrors(List(columnError(3, 22)), source, Map("regex" -> "^[ai]sInstanceOf$"))
  }

  @Test def testErrors2(): Unit = {
    val source = """
import collection.mutable._
object foo {
  def bar(x: Any) = new ArrayList[Int]()
}"""

    assertErrors(List(columnError(2, 18), columnError(4, 24)), source, Map("regex" -> "^ArrayList|ArrayBuffer|mutable$"))
  }

  @Test def testOk(): Unit = {
    val source = """
object foo {
  /** asInstanceOf[Int] is not used */
  def bar(x: Any) = 1
}"""

    assertErrors(List(), source, Map("regex" -> "^[ai]sInstanceOf$"))
  }

}
