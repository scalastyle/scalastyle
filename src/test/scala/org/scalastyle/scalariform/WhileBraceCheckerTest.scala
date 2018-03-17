// Copyright (C) 2011-2018 the original author or authors.
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

class WhileBraceCheckerTest extends AssertionsForJUnit with CheckerTest {

  override protected val key = "while.brace"

  override protected val classUnderTest = classOf[WhileBraceChecker]

  @Test def testOk(): Unit = {
    val source = """
package foobar

class Foobar {
  var acc = 1
  while (acc < 10) {
    acc += acc
  }
}
"""
    assertErrors(Nil, source)
  }

  @Test def testOkMultiline(): Unit = {
    val source = """
package foobar

class Foobar {
  var acc = 1
  while (acc < 10) {
    acc += acc
    acc -= 1
  }
}
"""
    assertErrors(Nil, source)
  }

  @Test def testOkMultiline2(): Unit = {
    val source = """
package foobar

class Foobar {
  var acc = 1
  while (acc < 10)
  {
    acc += acc
    acc -= 1
  }
}
"""
    assertErrors(Nil, source)
  }

  @Test def testFail(): Unit = {
    val source = """
package foobar

class Foobar {
  var acc = 1
  while (acc < 10)
    acc += acc
}
"""
    assertErrors(List(columnError(7, 4)), source)
  }

  @Test def testFail2(): Unit = {
    val source = """
package foobar

class Foobar {
  var acc = 1
  while (acc < 10)
    while (hhh > 4)
      acc += acc

}
"""
    assertErrors(List(columnError(7, 4), columnError(8, 6)), source)
  }
}
