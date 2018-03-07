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

class ForBraceCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "for.brace"
  val classUnderTest = classOf[ForBraceChecker]

  @Test def testOK(): Unit = {
    val source = """
package foobar

class Foobar {
  for ( t <- List(1,2,3)) yield t
  for { t <- List(1,2,3)} yield t
  for ( t <- List(1,2,3)) {
    printf("(%d)", t)
  }
}
"""

    assertErrors(List(), source, Map("singleLineAllowed" -> "true"))
  }

  @Test def testKOEnabled(): Unit = {
    val source = """
package foobar

class Foobar {
  for ( t <- List(1,2,3)) yield t
  for (
    t <- List(1,2,3)
  ) yield t
  for (
    t <- List(1,2,3);
    s <- List(4,5,6)
  ) yield t++s
  for { t <- List(1,2,3)} yield t
}
"""

    assertErrors(List(columnError(6, 6), columnError(9, 6)),
      source, Map("singleLineAllowed" -> "true"))
  }

  @Test def testKODisabled(): Unit = {
    val source = """
package foobar

class Foobar {
  for ( t <- List(1,2,3)) yield t
  for (
    t <- List(1,2,3)
  ) yield t
  for (
    t <- List(1,2,3);
    s <- List(4,5,6)
  ) yield t++s
  for { t <- List(1,2,3)} yield t
}
"""

    assertErrors(List(columnError(5, 6), columnError(6, 6), columnError(9, 6)), source)
  }
}
