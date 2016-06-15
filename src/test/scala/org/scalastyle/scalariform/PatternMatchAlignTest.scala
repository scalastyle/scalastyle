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

// scalastyle:off magic.number multiple.string.literals

class PatternMatchAlignTest extends AssertionsForJUnit with CheckerTest {
  protected val classUnderTest = classOf[PatternMatchAlignChecker]
  protected val key = "pattern.match.align"

  @Test def testNotAligned(): Unit = {
    val source =
    """
      |object foo {
      |  val a = Some (4)
      |
      |  a match {
      |    case Some(_) => "yes"
      |    case _ => "no"
      |  }
      |}
    """.stripMargin

    assertErrors(List(columnError(7, 11)), source)
  }


  @Test def testOk(): Unit = {
    val source =
    """
      |object foo {
      |  val a = Some (4)
      |
      |  a match {
      |    case Some(_) => "yes"
      |    case _       => "no"
      |  }
      |}
    """.stripMargin

    assertErrors(List(), source)
  }

  @Test def testNested(): Unit = {
    val source =
    """
      |object foo {
      |  val a = Some (4)
      |
      |  a match {
      |    case Some(x) => x match {
      |      case 3 => "no"
      |      case 4  => "yrs"
      |    }
      |    case _       => "no"
      |  }
      |}
    """.stripMargin

    assertErrors(List(columnError(8, 14)), source)
  }

}
