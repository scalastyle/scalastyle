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

import org.scalastyle.file.CheckerTest
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.scalastyle.Checker
import org.scalastyle.StyleError
import java.util.Set
import org.junit.Before
import org.junit.Test

// scalastyle:off magic.number multiple.string.literals

class CyclomaticComplexityCheckerTest extends AssertionsForJUnit with CheckerTest {
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
}
