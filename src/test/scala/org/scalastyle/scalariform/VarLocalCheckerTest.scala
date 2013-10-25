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

// scalastyle:off magic.number

class VarLocalCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "var.local"
  val classUnderTest = classOf[VarLocalChecker]

  @Test def testOne(): Unit = {
    val source = """
class C1 {
  var f1 = 1
  val f2 = 1
  var f3
  def m1() = {
    var v1 = 1
    v1
  }
  def m2() = {
    val v2 = 1
    def m3() = {
      var v3 = 1
      v3
    }
    m3()
  }
  val f4 = { (a: Int) =>
    var v4 = a
    v4
  }
  def m5() = {
    var v5, v6
    1
  }
}
object O1 {
  var f5 = 1
}
""";

    assertErrors(List(columnError(7, 4), columnError(13, 6), columnError(19, 4), columnError(23, 4)), source)
  }
}
