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

// scalastyle:off magic.number

class DeprecatedJavaCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "deprecated.java"
  val classUnderTest = classOf[DeprecatedJavaChecker]

  @Test def testClassOK(): Unit = {
    val source = """
package foobar

class OK {
  @Deprecated
  def noparameters(): Any = null

  @Deprecated("foobar")
  def parameters(): Any = null

  @java.lang.Deprecated
  def noparametersFull(): Any = null

  @java.lang.Deprecated("foobar")
  def parametersFull(): Any = null

  @deprecated
  def ok(): Any = null
}
"""

    assertErrors(List(columnError(5, 2), columnError(8, 2), columnError(11, 2), columnError(14, 2)), source)
  }
}

class OverrideJavaCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "override.java"
  val classUnderTest = classOf[OverrideJavaChecker]

  @Test def testClassOK(): Unit = {
    val source = """
package foobar

class FooBar {
  @Override
  def fun() = 1

  @java.lang.Override
  def fun2() = 2

  override def fun3() = 3
}
"""

    assertErrors(List(columnError(5, 2), columnError(8, 2)), source)
  }
}
