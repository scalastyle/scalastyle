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

class HeaderMatchesCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "header.matches"
  val classUnderTest = classOf[HeaderMatchesChecker]

  val licence = """/**
 * Copyright (C) 2009-2010 the original author or authors.
 * See the notice.md file distributed with this work for additional
 * information regarding copyright ownership.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */"""

  @Test def testOK(): Unit = {
    val source = licence + """
package foobar

  object Foobar {
}
""";

    assertErrors(List(), source, Map("header" -> licence))
  }

  @Test def testKO(): Unit = {
    val source = licence.replaceAll("BASIS,", "XXX") + """
package foobar

  object Foobar {
}
""";

    assertErrors(List(lineError(13)), source, Map("header" -> licence))
  }

  @Test def testTooShort(): Unit = {
    val source = """/**
 * Copyright (C) 2009-2010 the original author or authors.
 * See the notice.md file distributed with this work for additional
 * information regarding copyright ownership.""";

    assertErrors(List(lineError(4)), source, Map("header" -> licence))
  }

  val licenceRegex = {
    def literalOK(c: Char): Boolean = c match {
      case ' '|'-'|':'|'/'|'\n' => true
      case ld if ld.isLetterOrDigit => true
      case _ => false
    }
    (licence flatMap { c => if (literalOK(c)) c.toString else "\\" + c}).replace("2009-2010", "(?:\\d{4}-)?\\d{4}")
  }

  @Test def testRegexOK(): Unit = {
    val source = licence + """
package foobar

  object Foobar {
}
"""

    assertErrors(List(), source, Map("header" -> licenceRegex, "regex" -> "true"))
  }

  @Test def testRegexFlexible(): Unit = {
    val source = licence.replace("2009-2010", "2009-2014") + """
package foobar

  object Foobar {
}
"""

    assertErrors(List(), source, Map("header" -> licenceRegex, "regex" -> "true"))
  }

  @Test def testRegexKO(): Unit = {
    val source = licence.replace("2009-2010", "xxxx-xxxx") + """
package foobar

  object Foobar {
}
"""

    assertErrors(List(fileError()), source, Map("header" -> licenceRegex, "regex" -> "true"))
  }

}
