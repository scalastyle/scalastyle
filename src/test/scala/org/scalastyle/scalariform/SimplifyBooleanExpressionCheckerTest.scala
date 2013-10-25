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

class SimplifyBooleanExpressionCheckerTest extends AssertionsForJUnit with CheckerTest {
  protected val classUnderTest = classOf[SimplifyBooleanExpressionChecker]
  protected val key = "simplify.boolean.expression"

  @Test def testEquals(): Unit = {
    val source = """
package foobar

object Foobar {
  val b = true
  val foo01 = (b == true)
  val foo02 = (b != true)
  val foo03 = (b == false)
  val foo04 = (b != false)
}"""

    assertErrors(List(columnError(6, 15), columnError(7, 15), columnError(8, 15), columnError(9, 15)), source)
  }


  @Test def testErrors(): Unit = {
    val source = """
package foobar

object Foobar {
  val b = true
  val foo01 = (b == true)
  val foo02 = !false
  val foo03 = !true
}"""

    assertErrors(List(columnError(6, 15), columnError(7, 14), columnError(8, 14)), source)
  }

  @Test def testErrors2(): Unit = {
    val source = """
package foobar

object Foobar {
  val b = true
  val foo04 = b && true
  val foo05 = true && b
  val foo06 = b && false
  val foo07 = false && b
}"""

    assertErrors(List(columnError(6, 14), columnError(7, 14), columnError(8, 14), columnError(9, 14)), source)
  }

  @Test def testErrors3(): Unit = {
    val source = """
package foobar

object Foobar {
  val b = true
  val foo08 = b || true
  val foo09 = true || b
  val foo10 = b || false
  val foo11 = false || b
}"""

    assertErrors(List(columnError(6, 14), columnError(7, 14), columnError(8, 14), columnError(9, 14)), source)
  }

  @Test def testOK(): Unit = {
    val source = """
package foobar

object Foobar {
  val b = true
  val foo12 = b && b // doesn't match
  val foo13 = (b && b) || b
  val foo14 = b && (true)
}"""

    assertErrors(List(columnError(8, 14)), source)
  }
}
