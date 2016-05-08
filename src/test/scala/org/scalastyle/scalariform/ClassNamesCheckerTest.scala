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

// scalastyle:off magic.number multiple.string.literals

class ClassNamesCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "class.name"
  val classUnderTest = classOf[ClassNamesChecker]

  @Test def testZero(): Unit = {
    val source =
    """
      |package foobar
      |
      |class Foobar {
      |  val foo = 1
      |}
    """.stripMargin

    assertErrors(List(), source)
  }

  @Test def testOne(): Unit = {
    val source =
    """
      |package foobar
      |
      |class foobar {
      |  class barbar {
      |  }
      |}
    """.stripMargin

    assertErrors(List(columnError(4, 6, List("^[A-Z][A-Za-z]*$")), columnError(5, 8, List("^[A-Z][A-Za-z]*$"))), source)
  }
}

class ObjectNamesCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "object.name"
  val classUnderTest = classOf[ObjectNamesChecker]

  @Test def testZero(): Unit = {
    val source =
    """
      |package foobar
      |
      |object Foobar {
      |  val foo = 1
      |}
    """.stripMargin

    assertErrors(List(), source)
  }

  @Test def testOne(): Unit = {
    val source =
    """
      |package foobar
      |
      |object foobar {
      |  object barbar {
      |  }
      |}
    """.stripMargin

    assertErrors(List(columnError(4, 7, List("^[A-Z][A-Za-z]*$")), columnError(5, 9, List("^[A-Z][A-Za-z]*$"))), source)
  }

  @Test def testPackageObject(): Unit = {
    val source =
    """
      |package foobar
      |
      |package object foobar {
      |  object barbar {
      |  }
      |}
    """.stripMargin

    assertErrors(List(columnError(5, 9, List("^[A-Z][A-Za-z]*$"))), source)
  }
}


class PackageNamesCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "package.name"
  val classUnderTest = classOf[PackageNamesChecker]

  @Test def testSinglePartNoError(): Unit = {
    val source =
    """
      |package foobar
    """.stripMargin

    assertErrors(List(), source)
  }

  @Test def testSinglePartError(): Unit = {
    val source =
    """
      |package FooBar
    """.stripMargin

    assertErrors(List(columnError(2, 8, List("^[a-z][A-Za-z]*$"))), source)
  }

  @Test def testMultiPartNoError(): Unit = {
    val source =
    """
      |package abc.foobar
    """.stripMargin

    assertErrors(List(), source)
  }

  @Test def testMultiPartError(): Unit = {
    val source =
    """
      |package abc.foo_bar
    """.stripMargin

    assertErrors(List(columnError(2, 12, List("^[a-z][A-Za-z]*$"))), source)
  }

  @Test def testPackageObjectNoError(): Unit = {
    val source =
    """
      |package object _foo_bar
    """.stripMargin

    assertErrors(List(), source)
  }

  // Check case where the package name is built up by successive package statements.
  @Test def testMultiLinePackageNoError(): Unit = {
    val source =
    """
      |package foo
      |package bar
    """.stripMargin

    assertErrors(List(), source)
  }

  // Check case where the package name is built up by successive package statements.
  @Test def testMultiLinePackageError(): Unit = {
    val source =
    """
      |package foo
      |package Bar
    """.stripMargin

    assertErrors(List(columnError(3, 8, List("^[a-z][A-Za-z]*$"))), source)
  }

  @Test def testMultiLinePackageMultipleError(): Unit = {
    val source =
    """
      |package Foo
      |package Bar
    """.stripMargin

    assertErrors(List(columnError(2, 8, List("^[a-z][A-Za-z]*$")), columnError(3, 8, List("^[a-z][A-Za-z]*$"))), source)
  }

  @Test def testPackageAndPackageObjectNoError(): Unit = {
    val source =
    """
      |package org.thisone
      |package object _foo
    """.stripMargin

    assertErrors(List(), source)
  }

}

class PackageObjectNamesCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "package.object.name"
  val classUnderTest = classOf[PackageObjectNamesChecker]

  @Test def testZero(): Unit = {
    val source =
    """
      |package foobar
      |
      |package object foobar {
      |  val foo = 1
      |}
    """.stripMargin

    assertErrors(List(), source)
  }

  @Test def testOne(): Unit = {
    val source =
    """
      |package foobar
      |
      |package object Foobar {
      |}
      |package object Barbar {
      |}
    """.stripMargin

    assertErrors(List(columnError(4, 15, List("^[a-z][A-Za-z]*$")), columnError(6, 15, List("^[a-z][A-Za-z]*$"))), source)
  }

  @Test def testPackageObject(): Unit = {
    val source =
    """
      |package foobar
      |
      |object foobar {
      |  object barbar {
      |  }
      |}
    """.stripMargin

    assertErrors(List(), source)
  }
}

class MethodNamesCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "method.name"
  val classUnderTest = classOf[MethodNamesChecker]

  @Test def testDefault(): Unit = {
    val source =
    """
      |package foobar
      |
      |class Foobar {
      |  def foo() = 1
      |  def +() = 1
      |//  def foo+() = 1
      |  def setting_=(s: Boolean) {}
      |}
    """.stripMargin

    assertErrors(List(defErr(6, 6)), source)
  }

  @Test def testNonDefault(): Unit = {
    val source =
    """
      |package foobar
      |
      |class Foobar {
      |  def Foo() = 1
      |  def +() = 1
      |//  def Foo+() = 1
      |}
    """.stripMargin

    assertErrors(List(columnError(6, 6, List("^F[o*]*$"))), source, Map("regex" -> "^F[o*]*$"))
  }

  @Test def testWithIgnoreRegex(): Unit = {
    val source =
    """
      |package foobar
      |
      |class Foobar {
      |  def foo() = 1
      |  def +() = 1
      |  def -() = 1
      |//  def Foo+() = 1
      |}
    """.stripMargin

    assertErrors(List(defErr(7, 6)), source, Map("ignoreRegex" -> "^\\+$"))
  }

  @Test def testIgnoreOverride(): Unit = {
    val source =
    """
      |package foobar
      |
      |trait Bar {
      |  def +() = 1
      |  def -() = 1
      |  def &() = 1
      |}
      |
      |class Foobar extends Bar {
      |  override def +() = 1
      |  protected override def &() = 1
      |  override protected def -() = 1
      |  def bar() = 1
      |}
    """.stripMargin

    assertErrors(List(defErr(5, 6), defErr(6, 6), defErr(7, 6)), source, Map("ignoreOverride" -> "true"))
  }

  private def defErr(line: Int, column: Int) = columnError(line, column, List("^[a-z][A-Za-z0-9]*(_=)?$"))
}

class FieldNamesCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "field.name"
  val classUnderTest = classOf[FieldNamesChecker]

  @Test def testValidFieldNames(): Unit = {
    val source =
      """
        |package foobar
        |
        |class foobar {
        |  val myField1 = "one"
        |  var myField2 = 2
        |  val myField3
        |  var myField4
        |  val myField51; val myField52 = 52
        |  var myField61; var myField62 = 62
        |}
      """.stripMargin

    assertErrors(List(), source)
  }

  @Test def testInvalidFieldNames(): Unit = {
    val source =
      """
        |package foobar
        |
        |class foobar {
        |  val MyField1 = "one"
        |  var MyField2 = 2
        |  val MyField3
        |  var MyField4
        |  val myField51; val MyField52 = 52
        |  var myField61; var MyField62 = 62
        |}
      """.stripMargin

    assertErrors(
      List(
        columnError(5, 6, List("^[a-z][A-Za-z0-9]*$")),
        columnError(6, 6, List("^[a-z][A-Za-z0-9]*$")),
        columnError(7, 6, List("^[a-z][A-Za-z0-9]*$")),
        columnError(8, 6, List("^[a-z][A-Za-z0-9]*$")),
        columnError(9, 21, List("^[a-z][A-Za-z0-9]*$")),
        columnError(10, 21, List("^[a-z][A-Za-z0-9]*$"))),
      source)
  }
}
