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

class PublicMethodsHaveTypeCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "public.methods.have.type"
  val classUnderTest = classOf[PublicMethodsHaveTypeChecker]

  @Test def testClassOK(): Unit = {
    val source = """
package foobar

class OK {
  def c1() = 5
  def c2(): Int = 5
  def c3 = 5
  protected def c4() = 5
  private def c5() = 5
  private[this] def c6() = 5
  private val foo1 = 1
  val foo2 = 2
  def unit = {}
  def unit2 {}
  val foo = new scala.collection.mutable.HashMap {def foobar() = {}}
  def bar() = { new scala.collection.mutable.HashMap {def foobar() = {}} }
  def bar2() = new scala.collection.mutable.HashMap {def foobar2() = {}}
}
""";

    assertErrors(List(columnError(5, 6), columnError(7, 6), columnError(13, 6), columnError(15, 54),
                        columnError(16, 6), columnError(16, 58), columnError(17, 6), columnError(17, 57)), source)
  }

  @Test def testProc(): Unit = {
    val source = """
class classOK {
  def proc1 {}
  def proc2(): Unit = {}
}

abstract class abstractOK {
  def proc1 {}
  def proc2(): Unit = {}
  def proc3()
}

trait traitOK {
  def proc1 {}
  def proc2(): Unit = {}
  def proc3()
}
"""

    assertErrors(List(), source)
  }

  @Test def testConstructor(): Unit = {
    val source = """
class ConstructorOK(a: Int) {
  def this() = this(1)
}
"""

    assertErrors(List(), source)
  }

  @Test def testClassOverride(): Unit = {
    val source = """
package foobar

trait Foobar {
  def foobar: Int
}

class Sub extends Foobar {
  override def foobar() = 5
}
""";

    assertErrors(List(), source, Map("ignoreOverride" -> "true"))
    assertErrors(List(columnError(9, 15)), source, Map("ignoreOverride" -> "false"))
    assertErrors(List(columnError(9, 15)), source)
  }
}
