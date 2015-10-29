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

class CyclomaticComplexityCheckerTestHelper extends AssertionsForJUnit with CheckerTestHelper {
  val key = "cyclomatic.complexity"
  val classUnderTest = classOf[CyclomaticComplexityChecker]

  @Test def testKO(): Unit = {
    val source = """
package foobar

class Foobar {
  def foobar(i: Int): Int = {
    if (i == 1) {
      5
    } else if (i == 2) {
      true && false || true
      5 match {
        case 4 =>
        case 5 =>
        case _ =>
      }
    } else {
      var f = 0
      while (f > 0) {}
      do {} while (f > 0)
      for (t <- List())
      3
    }
  }

  def barbar(i: Int): Int = {
    if (i == 1) {
      5
    } else if (i == 2) {
      true && false || true
      5 match {
        case 4 =>
        case 5 =>
        case _ =>
      }
    } else {
      var f = 0
      while (f > 0) {}
      do {} while (f > 0)
      for (t <- List())
      3
    }
  }
}
""";

    assertErrors(List(columnError(5, 6, List("12", "11")), columnError(24, 6, List("12", "11"))), source, Map("maximum" -> "11"))
  }

  @Test def testEmbeddedMethods(): Unit = {
    val source = """
package foobar

class Foobar {
  def foobar(i: Int): Int = {
    // 1 for method, 2 for inner methods and 3 for if/elseif clause
    def bar1(i: Int) = if (i == 1) 1 else 2
    def bar2(i: Int) = if (i == 2) 1 else 2

    if (i == 1) {
      1
    } else if (i == 2) {
      2
    } else if (i == 3) {
      3
    } else {
      4
    }
  }

  def barbar(i: Int): Int = {
    // 1 for method, 2 for inner methods and 3 for if/elseif clause
    def bar1(i: Int) = {
      if (i == 1) {
        1
      } else if (i == 2) {
        2
      } else if (i == 3) {
        3
      } else {
        4
      }
    }

  }
}
"""

    assertErrors(List(columnError(5, 6, List("4", "3")), columnError(21, 6, List("4", "3"))), source, Map("maximum" -> "3"))
  }

  @Test def testEmbeddedClasses(): Unit = {
    val source = """
package foobar

class Foobar {
  // This is not caught by the checker. Don't know whether it should or not.
  val f = if (i == 1) {
    1
  } else if (i == 2) {
    2
  } else if (i == 3) {
    3
  } else {
    4
  }

  def foobar(i: Int): Int = {
    // 1 for method, 2 for inner methods and 3 for if/elseif clause
    class Foo2 {
      def bar1(i: Int) = if (i == 1) 1 else 2
      def bar2(i: Int) = if (i == 2) 1 else 2
    }

    if (i == 1) {
      1
    } else if (i == 2) {
      2
    } else if (i == 3) {
      3
    } else {
      4
    }
  }
}
"""

    assertErrors(List(columnError(16, 6, List("4", "3"))), source, Map("maximum" -> "3"))
  }
}
