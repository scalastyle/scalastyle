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

class EmptyClassCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "empty.class"
  val classUnderTest = classOf[EmptyClassChecker]

  @Test def testKO() {
    val source = """
package foobar

class Foobar1 {}
class Foobar2 { /* foobar */ }
class Foobar3 {
      // foobar
}
class Foobar4 { }
class Foobar5 {
}
class Foobar6 {
  def foobar() = 4
}
class Foobar7
""";

    assertErrors(List(columnError(4, 6), columnError(5, 6), columnError(6, 6), columnError(9, 6), columnError(10, 6)), source)
  }

  @Test def testInnerClass() {
    val source = """
package foobar

class Outer {
  class Foobar1
  class Foobar2 {}
  trait Barbar {}
}
""";

    assertErrors(List(columnError(6, 8), columnError(7, 8)), source)
  }
}
