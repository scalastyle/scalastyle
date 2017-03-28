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

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

// scalastyle:off magic.number multiple.string.literals

class HeaderMatchesCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "header.matches"
  val classUnderTest = classOf[HeaderMatchesChecker]

  val licenseLines = List(
    "/**",
    " * Copyright (C) 2009-2010 the original author or authors.",
    " * See the notice.md file distributed with this work for additional",
    " * information regarding copyright ownership.",
    " *",
    " * Licensed under the Apache License, Version 2.0 (the \"License\");",
    " * you may not use this file except in compliance with the License.",
    " * You may obtain a copy of the License at",
    " *",
    " *     http://www.apache.org/licenses/LICENSE-2.0",
    " *",
    " * Unless required by applicable law or agreed to in writing, software",
    " * distributed under the License is distributed on an \"AS IS\" BASIS,",
    " * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.",
    " * See the License for the specific language governing permissions and",
    " * limitations under the License.",
    " */"
  )
  val licenseUnix = licenseLines.mkString("\n")
  val licenseWin = licenseLines.mkString("\r\n")

  val baseSourceLines = List(
    "",
    "package foobar",
    "",
    "  object Foobar {",
    "}",
    ""
  )

  @Test def testOK(): Unit = {
    val sourceLines = licenseLines ::: baseSourceLines
    val sourceUnix = sourceLines.mkString("\n")
    val sourceWin = sourceLines.mkString("\r\n")
    assertErrors(List(), sourceUnix, Map("header" -> licenseUnix))
    assertErrors(List(), sourceWin, Map("header" -> licenseWin))
    assertErrors(List(), sourceUnix, Map("header" -> licenseWin))
    assertErrors(List(), sourceWin, Map("header" -> licenseUnix))
  }

  @Test def testKO(): Unit = {
    val sourceLines = licenseLines ::: baseSourceLines
    val sourceUnix = sourceLines.mkString("\n").replaceAll("BASIS,", "XXX")
    val sourceWin = sourceLines.mkString("\r\n").replaceAll("BASIS,", "XXX")

    assertErrors(List(lineError(13)), sourceUnix, Map("header" -> licenseUnix))
    assertErrors(List(lineError(13)), sourceWin, Map("header" -> licenseWin))
    assertErrors(List(lineError(13)), sourceUnix, Map("header" -> licenseWin))
    assertErrors(List(lineError(13)), sourceWin, Map("header" -> licenseUnix))
  }

  @Test def testTooShort(): Unit = {
    val shortSourceLines = licenseLines.take(4)
    val shortSourceUnix = shortSourceLines.mkString("\n")
    val shortSourceWin = shortSourceLines.mkString("\r\n")
    assertErrors(List(lineError(4)), shortSourceUnix, Map("header" -> licenseUnix))
    assertErrors(List(lineError(4)), shortSourceWin, Map("header" -> licenseWin))
    assertErrors(List(lineError(4)), shortSourceUnix, Map("header" -> licenseWin))
    assertErrors(List(lineError(4)), shortSourceWin, Map("header" -> licenseUnix))
  }

  def literalOK(c: Char): Boolean = c match {
    case ' '|'-'|':'|'/'|'\n' => true
    case ld: Any if ld.isLetterOrDigit => true
    case _ => false
  }

  val licenceRegexUnix = {
    (licenseUnix flatMap { c => if (literalOK(c)) c.toString else "\\" + c}).replace("2009-2010", "(?:\\d{4}-)?\\d{4}")
  }
  val licenceRegexWin = {
    (licenseWin flatMap { c => if (literalOK(c)) c.toString else "\\" + c}).replace("2009-2010", "(?:\\d{4}-)?\\d{4}")
  }

  @Test def testRegexOK(): Unit = {
    val sourceLines = licenseLines ::: baseSourceLines
    val sourceUnix = sourceLines.mkString("\n")
    val sourceWin = sourceLines.mkString("\r\n")

    assertErrors(List(), sourceUnix, Map("header" -> licenceRegexUnix, "regex" -> "true"))
    assertErrors(List(), sourceWin, Map("header" -> licenceRegexWin, "regex" -> "true"))
    assertErrors(List(), sourceUnix, Map("header" -> licenceRegexWin, "regex" -> "true"))
    assertErrors(List(), sourceWin, Map("header" -> licenceRegexUnix, "regex" -> "true"))
  }

  @Test def testRegexFlexible(): Unit = {
    val sourceLines = licenseLines ::: baseSourceLines
    val sourceUnix = sourceLines.mkString("\n").replace("2009-2010", "2009-2014")
    val sourceWin = sourceLines.mkString("\r\n").replace("2009-2010", "2009-2014")

    assertErrors(List(), sourceUnix, Map("header" -> licenceRegexUnix, "regex" -> "true"))
    assertErrors(List(), sourceWin, Map("header" -> licenceRegexWin, "regex" -> "true"))
    assertErrors(List(), sourceUnix, Map("header" -> licenceRegexWin, "regex" -> "true"))
    assertErrors(List(), sourceWin, Map("header" -> licenceRegexUnix, "regex" -> "true"))
  }

  @Test def testRegexKO(): Unit = {
    val sourceLines = licenseLines ::: baseSourceLines
    val sourceUnix = sourceLines.mkString("\n").replace("2009-2010", "xxxx-xxxx")
    val sourceWin = sourceLines.mkString("\r\n").replace("2009-2010", "xxxx-xxxx")

    assertErrors(List(fileError()), sourceUnix, Map("header" -> licenceRegexUnix, "regex" -> "true"))
    assertErrors(List(fileError()), sourceWin, Map("header" -> licenceRegexWin, "regex" -> "true"))
    assertErrors(List(fileError()), sourceUnix, Map("header" -> licenceRegexWin, "regex" -> "true"))
    assertErrors(List(fileError()), sourceWin, Map("header" -> licenceRegexUnix, "regex" -> "true"))
  }

}
