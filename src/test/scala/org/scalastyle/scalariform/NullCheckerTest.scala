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
import org.scalatestplus.junit.AssertionsForJUnit

// scalastyle:off magic.number

class NullCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "null"
  val classUnderTest = classOf[NullChecker]

  @Test def testZero(): Unit = {
    val source = """
package foobar

object Foobar {
  val foo = 1
}
"""

    assertErrors(List(), source)
  }

  @Test def testOne(): Unit = {
    val source = """
package foobar

object Foobar {
  val foo: String = null
  val bar: String = null
}
"""

    assertErrors(List(columnError(5, 20), columnError(6, 20)), source)
  }

  @Test def testTwo(): Unit = {
    val source = """
package foobar

object Foobar {
  def bar(s: String): Int = {
    if (s == null) 0
    else if (s != null) 1
    else 2
  }
}
"""

    assertErrors(List(), source)
  }

  @Test def testThree(): Unit = {
    val source = """
package foobar

object Foobar {
  def bar(s: String): Int = {
    if (s == null) 0
    else if (s != null) 1
    else 2

    if (null == s) 0
    else if (null != s) 1
    else 2
  }
}
"""

    assertErrors(
      List(columnError(6, 13), columnError(7, 18), columnError(10, 8), columnError(11, 13)),
      source,
      Map("allowNullChecks" -> "false")
    )
  }
}
