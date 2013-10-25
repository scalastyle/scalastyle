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

class UppercaseLCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "uppercase.l"
  val classUnderTest = classOf[UppercaseLChecker]

  @Test def testOK(): Unit = {
    val source = """
package foobar

object Foobar {
  val foo1 = 3l
  val foo2 = 3L
  val foo3 = 3
  val foo4 = 65 + 3l
}
""";

    assertErrors(List(columnError(5, 13), columnError(8, 18)), source)
  }
}
