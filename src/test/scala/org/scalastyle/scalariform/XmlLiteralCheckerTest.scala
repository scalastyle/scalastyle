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

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import org.scalastyle.file.CheckerTestHelper

// scalastyle:off magic.number

class XmlLiteralCheckerTest extends AssertionsForJUnit with CheckerTestHelper {

  protected val classUnderTest = classOf[XmlLiteralChecker]

  protected val key = "xml.literal"

  @Test def testZeroErrors(): Unit = {
    val source = """
class C1 {
  def m1(n: Int) = n
}
""";
    assertErrors(List(), source)
  }

  @Test def testOneError(): Unit = {
    val source = """
class C1 {
  val f = <foobar/>
}
"""
    assertErrors(List(columnError(3, 10)), source)
  }

  @Test def testTwoErrors(): Unit = {
    val source = """
class C1 {
  val f1 = <foobar/>
  val f2 = <foobar/>
}
"""
    assertErrors(List(columnError(3, 11), columnError(4, 11)), source)
  }

  @Test def testMultiLine(): Unit = {
    val source = """
class C1 {
  val f = <foobar>
            <foo>&gt;</foo>
          </foobar>
}
"""
    assertErrors(List(columnError(3, 10)), source)
  }
}
