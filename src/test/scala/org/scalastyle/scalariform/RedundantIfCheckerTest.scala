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

class RedundantIfCheckerTest extends AssertionsForJUnit with CheckerTest {
  protected val classUnderTest = classOf[RedundantIfChecker]
  protected val key = "if.redundant"

  @Test def testErrors(): Unit = {
    val source = """
package foobar

object Foobar {
  val b1 = true
  val b2 = if (b1) true else false
  val b3 = if (!b1) false else true
  val b4 =
      if (b2 && b3)
      true
      else false
  val b5 = if (b4) (if (b3) true else false) else b1
  val b6 =
      if (b1) true
      else if (b2) true
      else false
}"""

    assertErrors(List(columnError(6, 11), columnError(7, 11), columnError(9, 6), columnError(12, 20), columnError(15, 11)), source)
  }

  @Test def testOk(): Unit = {
    val source = """
package foobar

object Foobar {
  val b1 = true
  val b2 = false
  val b3 =
      if (b1) true
      else if (!b1 && b2) false
      else !b1
}"""

    assertErrors(List(), source)
  }

}
