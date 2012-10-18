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

class MultipleStringLiteralsCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "multiple.string.literals"
  val classUnderTest = classOf[MultipleStringLiteralsChecker]

  @Test def testParameters() {
    val source = """
package foobar

class Foobar {
  var a = "foobar"
  var b = "foobar"
  var c = "foobar"
  val d = "foobar"
  def e = "foobar"
  def f(f: String = "foobar") = 5

  var a1 = "bar"
  val d1 = "bar"
  def e1 = "bar"
  def f1(f: String = "bar") = "foobar"
}
""";

    assertErrors(List(columnError(5, 10, List(""""foobar"""", "7", "3")), columnError(12, 11, List(""""bar"""", "4", "3"))), source, Map("allowed" -> "3"))
  }

  @Test def testDefaultParameters() {
    val source = """
package foobar

class Foobar {
  var a = "foobar"
  var b = "foobar"

  var a1 = "bar"
}
""";

    assertErrors(List(columnError(5, 10, List(""""foobar"""", "2", "1"))), source)
  }

  @Test def testMultiLine() {
    val source = """
package foobar

class Foobar {
  var a = ###foobar
  oop###
  var b = ###foobar
  oop###
}
""".replace("###", "\"\"\"");

    assertErrors(List(columnError(5, 10, List(""""foobar
  oop"""", "2", "1"))), source)
  }

  @Test def testMultiLineAndNormal() {
    val source = """
package foobar

class Foobar {
  var a = ###foobar###
  var b = "foobar"
}
""".replace("###", "\"\"\"");

    assertErrors(List(columnError(5, 10, List(""""foobar"""", "2", "1"))), source)
  }
}
