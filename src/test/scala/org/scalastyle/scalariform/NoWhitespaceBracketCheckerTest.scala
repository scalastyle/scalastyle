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

class NoWhitespaceBeforeLeftBracketCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "no.whitespace.before.left.bracket"
  val classUnderTest = classOf[NoWhitespaceBeforeLeftBracketChecker]

  @Test def testOK(): Unit = {
    val source = """
package foobar

class Foobar[T] {
}
""";

    assertErrors(List(), source)
  }

  @Test def testOneSpace(): Unit = {
    val source = """
package foobar

class Foobar [T] {
}
""";
    assertErrors(List(columnError(4, 6)), source)
  }

  @Test def testTwoSpaces(): Unit = {
    val source = """
package foobar

class Foobar [ Barbar [T]] {
}
""";

    assertErrors(List(columnError(4, 6), columnError(4, 15)), source)
  }
}

class NoWhitespaceAfterLeftBracketCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "no.whitespace.after.left.bracket"
  val classUnderTest = classOf[NoWhitespaceAfterLeftBracketChecker]

  @Test def testOK(): Unit = {
    val source = """
package foobar

class Foobar[T] {
}
""";

    assertErrors(List(), source)
  }

  @Test def testOneSpace(): Unit = {
    val source = """
package foobar

class Foobar[ T] {
}
""";

    assertErrors(List(columnError(4, 12)), source)
  }

  @Test def testTwoSpaces(): Unit = {
    val source = """
package foobar

class Foobar[ Barbar[ T]] {
}
""";

    assertErrors(List(columnError(4, 12), columnError(4, 20)), source)
  }
}
