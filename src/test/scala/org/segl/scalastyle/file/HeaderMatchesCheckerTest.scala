package org.segl.scalastyle.file

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.segl.scalastyle.Checker
import org.segl.scalastyle.StyleError
import org.segl.scalastyle.Message

import java.util.Set;

import org.junit.Before;
import org.junit.Test;

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

  @Test def testOK() = {
    val source = licence + """
package foobar
import foobar
  object Foobar {
}
""";

    assertErrors(List(), source, Map("header" -> licence))
  }

  @Test def testKO() = {
    val source = licence.replaceAll("BASIS,", "XXX") + """
package foobar
import foobar
  object Foobar {
}
""";

    assertErrors(List(lineError(13)), source, Map("header" -> licence))
  }

  @Test def testTooShort() = {
    val source = """/**
 * Copyright (C) 2009-2010 the original author or authors.
 * See the notice.md file distributed with this work for additional
 * information regarding copyright ownership.""";

    assertErrors(List(lineError(4)), source, Map("header" -> licence))
  }
}
