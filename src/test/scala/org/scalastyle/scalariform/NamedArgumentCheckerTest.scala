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

// scalastyle:off magic.number

class NamedArgumentCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "named.argument"
  val classUnderTest = classOf[NamedArgumentChecker]

  @Test def testDefaultOK(): Unit = {
    val source = """
                   |package foobar
                   |
                   |class Foobar {
                   |b(b = true)
                   |i(i = 1)
                   |l(l = 1L)
                   |f(f = 1.2)
                   |c(c = 'a')
                   |n(n = null)
                   |h(x = foobar())
                   |}
                 """.stripMargin

    assertErrors(List.empty, source)
  }

  @Test def testDefaultKO(): Unit = {
    val source = """
                      |package foobar
                      |
                      |class Foobar {
                      |b(true)
                      |i(1)
                      |l(1L)
                      |f(1.2)
                      |c('a')
                      |n(null)
                      |h(foobar())
                      |}
                    """.stripMargin

    assertErrors(List(
      columnError(5, 2),
      columnError(6, 2),
      columnError(7, 2),
      columnError(8, 2),
      columnError(9, 2),
      columnError(10, 2)), source)
  }

  @Test def testStringOK(): Unit = {
    val source = """
                   |package foobar
                   |
                   |class Foobar {
                   |s1(s = "abc")
                   |s2(s = s"a$bc")
                   |}
                 """.stripMargin

    assertErrors(List.empty, source, Map("checkString" -> "true"))
  }

  @Test def testStringKO(): Unit = {
    val source = """
                      |package foobar
                      |
                      |class Foobar {
                      |s1("abc")
                      |s2(s"a${b}c")
                      |}
                    """.stripMargin

    assertErrors(List(
      columnError(5, 3),
      columnError(6, 3)), source, Map("checkString" -> "true"))
  }

  @Test def testIgnoreOK1(): Unit = {
    val source = """package foobar
                    |
                    |class Foobar {
                    |setF(1)
                    |}
                    |""".stripMargin

    assertErrors(List.empty, source)

    assertErrors(List(columnError(4, 5)), source, Map("ignoreMethod" -> "^f$"))
  }

  @Test def testIgnoreOK2(): Unit = {
    val source = """package foobar
                    |
                    |class Foobar {
                    |f(1)
                    |}
                    |""".stripMargin

    assertErrors(List(columnError(4, 2)), source)

    assertErrors(List.empty, source, Map("ignoreMethod" -> "^f$"))
  }

}
