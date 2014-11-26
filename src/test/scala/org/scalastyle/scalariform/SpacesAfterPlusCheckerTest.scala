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
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.scalastyle.Checker
import org.scalastyle.StyleError
import java.util.Set
import org.junit.Before
import org.junit.Test

// scalastyle:off magic.number

class SpacesAfterPlusCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "spaces.after.plus"
  val classUnderTest = classOf[SpacesAfterPlusChecker]

  @Test def testOK(): Unit = {
    val source = """
package foobar

object Foobar {
  val foo = 1 + 2
}
""";

    assertErrors(List(), source)
  }

  @Test def testNoSpaces(): Unit = {
    val source = """
package foobar

object Foobar {
  val foo = 1 +2
}

class Clazz[+T <: AstNode]() // ignore + within type specification
""";

    assertErrors(List(columnError(5, 14)), source)
  }

  @Test def testNoSpacesMultipleTypeParams(): Unit = {
    val source =
      """
        |package foobar
        |
        |class FooBar[+A, +B]
        |
        |object FooBar {
        |  type T[+R, +S] = FooBar[R, S]
        |}
      """.stripMargin

    assertErrors(List(), source)
  }

  @Test def testPlusMethodDef(): Unit = {
    val source =
      """
        |package foobar
        |
        |object FooBar {
        |  def +(): Unit = ()
        |}
    """.stripMargin

    assertErrors(List(), source)
  }

  @Test def testTwoSpaces(): Unit = {
    val source = """
package foobar

object Foobar {
  val foo = 1 +  2
}
""";

    assertErrors(List(), source)
  }

  @Test def testPlusAtEndOfLine(): Unit = {
    val source = """
package foobar

object Foobar {
  val someText = "f012345678901234567890123456789" +
  " some more text"
}
""";

    assertErrors(List(), source)
  }
}
