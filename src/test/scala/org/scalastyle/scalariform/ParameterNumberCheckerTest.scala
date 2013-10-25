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

class ParameterNumberCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "parameter.number"
  val classUnderTest = classOf[ParameterNumberChecker]

  @Test def testOK(): Unit = {
    val source = """
package foobar

class OK {
  def method(i1: Int, i2: Int, i3: Int, i4: Int, i5: Int, i6: Int, i7: Int, i8: Int): Int = 45
}
""";

    assertErrors(List(), source)
  }

  @Test def testKO(): Unit = {
    val source = """
package foobar

class OK {
  def method(i1: Int, i2: Int, i3: Int, i4: Int, i5: Int, i6: Int, i7: Int, i8: Int, i9: Int): Int = 45
}
""";

    assertErrors(List(columnError(5, 6, List("8"))), source)
  }

  @Test def testOuterKOInnerKO(): Unit = {
    val source = """
package foobar

class Outer {
  object Inner {
    def method(i1: Int, i2: Int, i3: Int, i4: Int, i5: Int, i6: Int, i7: Int, i8: Int, i9: Int): Int = 45
  }

  class Inner {
    def method(i1: Int, i2: Int, i3: Int, i4: Int, i5: Int, i6: Int, i7: Int, i8: Int, i9: Int): Int = 45
  }
}
""";

    assertErrors(List(columnError(6, 8, List("8")), columnError(10, 8, List("8"))), source)
  }
}
