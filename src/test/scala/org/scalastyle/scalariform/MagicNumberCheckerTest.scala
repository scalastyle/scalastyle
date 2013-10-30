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

class MagicNumberCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "magic.number"
  val classUnderTest = classOf[MagicNumberChecker]

  @Test def testVal(): Unit = {
    val source = """
package foobar

class Foobar {

  val foo0 = -2
  val foo1 = -1
  val foo2 = 0
  val foo3 = 1
  val foo4 = 2
  val foo5 = 3
  val foo6 = 4
}
""";

    assertErrors(List(), source)
  }

  @Test def testVar(): Unit = {
    val source = """
package foobar

class Foobar {
  var foo0 = -2
  var foo1 = -1
  var foo2 = 0
  var foo3 = 1
  var foo4 = 2
  var foo5 = 3
  var foo6 = 4
}
""";

    assertErrors(List(columnError(5, 13), columnError(10, 13), columnError(11, 13)), source)
  }

  @Test def testVar2(): Unit = {
    val source = """
package foobar

class Foobar {
  var foo6 = 4
  var foo7 = +4
  var foo8 = -4
  var bar1 = fn(7, -5)
  var bar2 = fn(1, -5)

  def fn(i: Int, j: Int) = i + j
}
""";

    assertErrors(List(columnError(5, 13), columnError(6, 13), columnError(7, 13), columnError(8, 16), columnError(8, 19), columnError(9, 19)), source)
  }

  @Test def testValLong(): Unit = {
    val source = """
package foobar

class Foobar {

  val foo0 = -2L
  val foo1 = -1L
  val foo2 = 0L
  val foo3 = 1L
  val foo4 = 2L
  val foo5 = 3L
  val foo6 = 4L
}
""";

    assertErrors(List(), source)
  }

  @Test def testVarLong(): Unit = {
    val source = """
package foobar

class Foobar {
  var foo0 = -2L
  var foo1 = -1L
  var foo2 = 0L
  var foo3 = 1L
  var foo4 = 2L
  var foo5 = 3L
  var foo6 = 4L
}
""";

    assertErrors(List(columnError(5, 13), columnError(10, 13), columnError(11, 13)), source)
  }

  @Test def testVar2Long(): Unit = {
    val source = """
package foobar

class Foobar {
  var foo6 = 4L
  var foo7 = +4L
  var foo8 = -4L
  var bar1 = fn(7L, -5L)
  var bar2 = fn(1L, -5L)

  def fn(i: Int, j: Int) = i + j
}
""";

    assertErrors(List(columnError(5, 13), columnError(6, 13), columnError(7, 13), columnError(8, 16), columnError(8, 20), columnError(9, 20)), source)
  }
}
