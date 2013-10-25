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

// scalastyle:off magic.number multiple.string.literals

class IfBraceCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "if.brace"
  val classUnderTest = classOf[IfBraceChecker]

  @Test def testDefault(): Unit = {
    val source = """
package foobar

class Foobar {
  val foo0 = if (true) "yes" else "no"
  val foo1 = if (true)
                 "yes"
                 else
                 "no"
  val foo2 = if (true) { "yes" } else { "no" }
  val foo3 = if (true) {
                 "yes"
                 } else {
                 "no"
                 }
}
""";

    assertErrors(List(columnError(6, 13)), source)
  }

  @Test def testDefault2(): Unit = {
    val source = """
package foobar

class Foobar {
  val foo4 = if (true) "yes"
                 else "no"
  val foo5 = if (true) "yes"
  val foo6 = if (true)
                "yes"
  val foo7 = if (true)
                !false
             else
                !true
}
""";

    assertErrors(List(columnError(5, 13), columnError(8, 13), columnError(10, 13)), source)
  }

  @Test def testSingleLineNotAllowed(): Unit = {
    val source = """
package foobar

class Foobar {
  val foo0 = if (true) "yes" else "no"
  val foo1 = if (true)
                 "yes"
                 else
                 "no"
  val foo2 = if (true) { "yes" } else { "no" }
  val foo3 = if (true) {
                 "yes"
                 } else {
                 "no"
                 }
}
""";

    assertErrors(List(columnError(5, 13), columnError(6, 13)), source, Map("singleLineAllowed" -> "false"))
  }

  @Test def testDoubleLine(): Unit = {
    val source = """
package foobar

class Foobar {
  val foo0 = if (true) "yes" else { "no" }
  val foo1 = if (true) "yes"
                 else "no"
}
""";

    assertErrors(List(), source, Map("doubleLineAllowed" -> "true"))
  }

  @Test def testElseIf(): Unit = {
    val source = """
package foobar

class Foobar {
  val foo0 = if (true) "yes" else if (true) "no" else "bar"
  val foo1 = if (true) "yes"
             else if (true) "no" else "bar"
  val foo1 = if (true) {
        "yes"
      } else if (true) {
        "no"
      } else {
        "bar"
      }
}
""";

    assertErrors(List(columnError(6, 13)), source)
  }
}
