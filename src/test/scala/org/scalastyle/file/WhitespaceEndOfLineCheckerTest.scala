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

package org.scalastyle.file

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.scalastyle.Checker
import org.scalastyle.StyleError
import org.scalastyle.Message
import java.util.Set;

import org.junit.Before;
import org.junit.Test;

// scalastyle:off magic.number multiple.string.literals

class WhitespaceEndOfLineCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "whitespace.end.of.line"
  val classUnderTest = classOf[WhitespaceEndOfLineChecker]

  @Test def testZero(): Unit = {
    val source = """
package foobar

object Foobar {
}
""";

    assertErrors(List(), source)
  }

  @Test def testOne(): Unit = {
    val source = """
package foobar##

object Foobar {
}
""".replaceAll("#", " ");

    assertErrors(List(columnError(2, 14)), source)
  }

  @Test def testTwo(): Unit = {
    val source = """
package foobar~
class  foobar#
object Foobar {
}
""".replaceAll("~", " ").replaceAll("#", "\t");

    assertErrors(List(columnError(2, 14), columnError(3, 13)), source)
  }
}
