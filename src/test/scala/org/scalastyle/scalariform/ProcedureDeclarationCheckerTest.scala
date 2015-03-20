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

class ProcedureDeclarationCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "procedure.declaration"
  val classUnderTest = classOf[ProcedureDeclarationChecker]

  @Test def testClassOK(): Unit = {
    val source = """
package foobar

abstract class OK {
  def c1() = 5
  def c2(): Int = 5
  def c3 = 5
  val foo2 = 2
  def unit = {}
  def unit2 {}
  def unit3
  def unit4: Unit
  def unit5(i: Int) = {}
  def unit6(i: Int) {}
  def unit7(i: Int)
  def unit8(i: Int): Unit
  val foo: Unit = new scala.collection.mutable.HashMap {def foobar() = {}}
  def bar() = { new scala.collection.mutable.HashMap {def foobar() = {}} }
  def bar2() = new scala.collection.mutable.HashMap {def foobar2() = {}}
  val bar3
}
""";

    assertErrors(List(columnError(10, 6), columnError(11, 6), columnError(14, 6), columnError(15, 6)), source)
  }

  @Test def testConstructor(): Unit = {
    val source = """
class ConstructorOK(a: Int) {
  def this() = this(1)
}
"""

    assertErrors(List(), source)
  }
}
