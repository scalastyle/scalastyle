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

class NumberOfTypesCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "number.of.types"
  val classUnderTest = classOf[NumberOfTypesChecker]

  @Test def testOK(): Unit = {
    val source = """
package foobar

case class F1()
case class F2()
case class F3()
case class F4()
case class F5()
case class F6()
""";

    assertErrors(List(), source, Map("maxTypes" -> "6"))
  }

  @Test def testKO(): Unit = {
    val source = """
package foobar

case class F1()
case class F2()
case class F3()
case class F4()
case class F5()
case class F6()
""";

    assertErrors(List(fileError(List("5"))), source, Map("maxTypes" -> "5"))
  }

  @Test def testInnerClasses(): Unit = {
    val source = """
package foobar

case class F1()
case class F2()
case class F3()
case class F4()
class F5() {
  class Foobar {
  }
}
case class F6()
""";

    assertErrors(List(fileError(List("6"))), source, Map("maxTypes" -> "6"))
  }
}
