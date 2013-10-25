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

class SpacesBeforePlusCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "spaces.before.plus"
  val classUnderTest = classOf[SpacesBeforePlusChecker]

  @Test def testOK(): Unit = {
    val source = """
package foobar

object Foobar {
  val foo = 12 + 2
}
class Clazz[+T <: AstNode]() // ignore + within type specification
""";

    assertErrors(List(), source)
  }

  @Test def testOne(): Unit = {
    val source = """
package foobar

object Foobar {
  val foo = 12+ 2
}
""";

    assertErrors(List(columnError(5, 14)), source)
  }

  @Test def testPlusAtBeginningOfLine(): Unit = {
    val source = """
package foobar

object Foobar {
  val foo = 12
+ 2
}
""";

    assertErrors(List(), source)
  }
}
