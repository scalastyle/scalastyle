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

// scalastyle:off magic.number multiple.string.literals

class EmptyInterpolatedStringCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "empty.interpolated.strings"
  val classUnderTest = classOf[EmptyInterpolatedStringChecker]

  @Test def testZero(): Unit = {
    val source = """
package foobar

class Foobar {
  val foo = "foo"
}
"""
    assertErrors(List(), source)
  }

  @Test def testCorrect(): Unit = {
    val source = """
package foobar

class Foobar {
  val foo = "foo"
  val bar = s"$foo bar"
}
"""
    assertErrors(List(), source)
  }

  @Test def testOne(): Unit = {
    val source = """
package foobar

class Foobar {
  val foo = s"foo"
}
"""
    assertErrors(List(columnError(5, 13)), source)
  }

  @Test def testMultiple(): Unit = {
    val source = """
package foobar

class Foobar {
  val foo = s"foo"
  val bar = s""
  val baz = s"   baz     "
}
"""
    assertErrors(List(columnError(5, 13), columnError(6, 13), columnError(7, 13)), source)
  }

  @Test def testMix(): Unit = {
    val source = """
package foobar

class Foobar {
  val foo = s"foo"
  val bar = s""
  val real = s"this is $foo real $bar"
  val baz = s"   baz     "
}
"""
    assertErrors(List(columnError(5, 13), columnError(6, 13), columnError(8, 13)), source)
  }

  @Test def testRaw(): Unit = {
    val source = """
package foobar

class Foobar {
  val foo = s"foo"
  val bar = raw"test"
}
"""
    assertErrors(List(columnError(5, 13)), source)

  }
}
