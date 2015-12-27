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
import org.scalastyle.file.CheckerTestHelper
import org.junit.Test

// scalastyle:off magic.number

class NotImplementedErrorUsageTest extends AssertionsForJUnit with CheckerTestHelper {

  val key = "not.implemented.error.usage"
  val classUnderTest = classOf[NotImplementedErrorUsage]

  @Test
  def noErrors(): Unit = {
    val source = """
class X {
  val x = 0
}
      """
    assertErrors(Nil, source)
  }

  @Test
  def notImplementedErrorFound(): Unit = {
    val source = """
class X {
  val x = ???
}
      """
    assertErrors(List(columnError(3, 10)), source)
  }
}
