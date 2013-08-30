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
import org.junit.Test
import org.scalastyle.ColumnError

// scalastyle:off magic.number

class NoWhitespaceBeforeColonCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "no.whitespace.before.colon"
  val classUnderTest = classOf[NoWhitespaceBeforeColonChecker]

  @Test def testClassStatementOK() {
    val source = """
package foobar

case class SomeClass(x: Int)

class Foobar(a: Int, b: Int) { }
                 """;

    assertErrors(List(), source)
  }

  @Test def testClassStatementKO() {
    val source = """
case class SomeClass(x : Int)
class Foobar(a : Int, b : Int) { }
                 """;

    assertErrors(List(columnError(2, 23), columnError(3, 15), columnError(3, 24)), source)
  }

  @Test def testDefAndValStatementOK() {
    val source = """
class Foobar() {
  def *++ : Int = 1
  val ***++: : Int= 1
  def foo() {
    val i: Int = 1
    val l = 1 :: List(2, 3)
  }
  def bar: Unit { }
  def baz(x: Int)(implicit y: Int): Int = x
  def foobar[T]: Int = 1
  def foobaz[T](x: Int): Int = 1
}
                 """;

    assertErrors(List(), source)
  }

  @Test def testDefAndValStatementKO() {
    val source = """
class Foobar() {
  def *++  : Int = 1
  val ***++:   : Int= 1
  def foo() {
    val i : Int = 1
    val l = 1 :: List(2, 3)
  }
  def bar : Unit { }
  def foobar[T] : Int = 1
  def foobaz[T](x : Int) : Int = 1
}
                 """;

    assertErrors(List(columnError(3, 11), columnError(4, 15), columnError(6, 10),
      columnError(9, 10), columnError(10, 16), columnError(11, 18), columnError(11, 25)), source)
  }

  @Test def testCaseStatementOK() {
    val source = """
case class Something()
case class Anything()

class Foobar() {
  def foo() {
    val l = List(Something, Something, Anything, Something, Anything).map{
      case i: Something => 2
      case s: Anything => 1
      case _ => 0
    }
  }
}
                 """;

    assertErrors(List(), source)
  }

  @Test def testCaseStatementKO() {
    val source = """
case class Something()
case class Anything()

class Foobar() {
  def foo() {
    val l = List(Something, Something, Anything, Something, Anything).map{
      case i : Something => 2
      case s : Anything => 1
      case _ => 0
    }
  }
}
                 """;

    assertErrors(List(columnError(8, 13), columnError(9, 13)), source)
  }

  @Test def testLineBreakAllowed() {
    val source = """
case class Something()
case class Anything()

class Foobar() {
  def bar(
    veryLongArgumentFoo: Int,
    veryLongArgumentBar: Int)
  : Unit {
    val a: Int = 0
    val b
    : Int = 0
  }
}
                 """;

    assertErrors(List(), source, Map("lineBreakAllowed" -> "true"))
  }

  @Test def testLineBreakNotAllowed() {
    val source = """
case class Something()
case class Anything()

class Foobar() {
  def bar(
    veryLongArgumentFoo: Int,
    veryLongArgumentBar: Int)
  : Unit {
    val a :
     Int = 0
    val b
    : Int = 0
  }
}
                 """;

    assertErrors(List(columnError(9, 2), columnError(10, 10), columnError(13, 4)), source, Map("lineBreakAllowed" -> "false"))
  }

}

class WhitespaceAfterColonCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "whitespace.after.colon"
  val classUnderTest = classOf[WhitespaceAfterColonChecker]

  @Test def testClassStatementOK() {
    val source = """
package foobar

case class SomeClass(x: Int)

class Foobar(a: Int, b: Int) { }
                 """;

    assertErrors(List(), source)
  }


  @Test def testClassStatementKO() {
    val source = """
case class SomeClass(x :Int)
class Foobar(a :Int, b :Int) { }
                 """;

    assertErrors(List(columnError(2, 23), columnError(3, 15), columnError(3, 23)), source)
  }

  @Test def testDefAndValStatementOK() {
    val source = """
class Foobar() {
  def *++ : Int = 1
  val ***++: : Int= 1
  def foo() {
    val i: Int = 1
    val l = 1 :: List(2, 3)
  }
  def bar: Unit { }
}
                 """;

    assertErrors(List(), source)
  }

  @Test def testDefAndValStatementKO() {
    val source = """
class Foobar() {
  def *++  :Int = 1
  val ***++:   :Int= 1
  def foo() {
    val i :Int = 1
    val l = 1 :: List(2, 3)
  }
  def bar :Unit { }
}
                 """;

    assertErrors(List(columnError(3, 11), columnError(4, 15), columnError(6, 10), columnError(9, 10)), source)
  }

  @Test def testCaseStatementOK() {
    val source = """
case class Something()
case class Anything()

class Foobar() {
  def foo() {
    val l = List(Something, Something, Anything, Something, Anything).map{
      case i: Something => 2
      case s: Anything => 1
      case _ => 0
    }
  }
}
                 """;

    assertErrors(List(), source)
  }

  @Test def testCaseStatementKO() {
    val source = """
case class Something()
case class Anything()

class Foobar() {
  def foo() {
    val l = List(Something, Something, Anything, Something, Anything).map{
      case i :Something => 2
      case s :Anything => 1
      case _ => 0
    }
  }
}
                 """;

    assertErrors(List(columnError(8, 13), columnError(9, 13)), source)
  }
}
