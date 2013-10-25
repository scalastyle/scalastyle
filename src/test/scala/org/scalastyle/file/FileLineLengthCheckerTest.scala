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

class FileLineLengthCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "line.size.limit"
  val classUnderTest = classOf[FileLineLengthChecker]

  @Test def testNoMax(): Unit = {
    val source = """
package foobar

    object Foobar {
}
""";

    assertErrors(List(), source, Map("maxLineLength" -> "20"))
  }

  @Test def testWithOneMax(): Unit = {
    val source = """
package foobar

    object Foobar {
}
""";

    assertErrors(List(lineError(4, List("15"))), source, Map("maxLineLength" -> "15"))
  }

  @Test def testWithImports(): Unit = {
    val source = """
package foobar
import org.scalastyle.file.SuperLongImportClass

    object Foobar {
      import org.scalastyle.file._
}
""";

    assertErrors(
      List(lineError(5, List("15"))),
      source,
      Map("maxLineLength" -> "15", "ignoreImports" -> "true"))

    assertErrors(
      List(lineError(3, List("15")), lineError(5, List("15")), lineError(6, List("15"))),
      source,
      Map("maxLineLength" -> "15"))
  }


  @Test def testWithTwoMax(): Unit = {
    val source = """
package foobar

    object Foobar {
}
    object Barbar {
}
""";

    assertErrors(List(lineError(4, List("15")), lineError(6, List("15"))), source, Map("maxLineLength" -> "15"))
  }

  @Test def testWithSpacesTabs(): Unit = {
    val source = """
package foobar

import# #java.lang._
object Barbar {
}
""".replaceAll("#","\t");

    assertErrors(List(lineError(4, List("14")), lineError(5, List("14"))), source, Map("maxLineLength" -> "14"))
  }
}
