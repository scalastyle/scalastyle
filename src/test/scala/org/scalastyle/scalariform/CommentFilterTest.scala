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
import org.scalastyle.file.CheckerTestHelper
import org.scalatest.junit.AssertionsForJUnit

// scalastyle:off magic.number multiple.string.literals

class CommentFilterTest extends AssertionsForJUnit with CheckerTestHelper {
  val key = "class.name"
  val classUnderTest = classOf[ClassNamesChecker]

  @Test def testOnOff(): Unit = {
    val source = """
package foobar

class foobar {
  // scalastyle:off
  class barbar { }
  // scalastyle:on
  // scalastyle:off class.name
  class bazbaz {}
  // scalastyle:on class.name

  // scalastyle:off object.name
  val s = " // scalastyle:off "
  class g { }
}
"""

    assertErrors(List(columnError(4, 6, List("^[A-Z][A-Za-z]*$")), columnError(14, 8, List("^[A-Z][A-Za-z]*$"))), source)
  }

  @Test def testOnOffIgnore(): Unit = {
    val source = """
package foobar

class foobar {
  // scalastyle:on class.name
  class barbar1 { } // scalastyle:ignore class.name
  //

  // scalastyle:on
  class barbar2 { } // scalastyle:ignore
  // scalastyle:off

  // scalastyle:on
  class barbar3 { } // scalastyle:ignore class.name
  // scalastyle:off

  // scalastyle:on
  class barbar4 { } // scalastyle:ignore magic.number
  // scalastyle:off
}
"""

    assertErrors(List(columnError(4, 6, List("^[A-Z][A-Za-z]*$")), columnError(18, 8, List("^[A-Z][A-Za-z]*$"))), source)
  }
}

class CommentFilterMagicNumberTest extends AssertionsForJUnit with CheckerTestHelper {
  val key = "magic.number"
  val classUnderTest = classOf[MagicNumberChecker]

  @Test def testMagicNumberFilter(): Unit = {
    val source =
      """
package foobar

object Foo {
  f(77) // scalastyle:ignore
  f(88)
  f(99)
}
"""

    assertErrors(List(columnError(6, 4, List()), columnError(7, 4, List())), source)
  }
}
