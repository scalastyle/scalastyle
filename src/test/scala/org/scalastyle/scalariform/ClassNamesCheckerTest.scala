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

class ClassNamesCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "class.name"
  val classUnderTest = classOf[ClassNamesChecker]

  @Test def testZero(): Unit = {
    val source = """
package foobar

class Foobar {
  val foo = 1
}
"""

    assertErrors(List(), source)
  }

  @Test def testOne(): Unit = {
    val source = """
package foobar

class foobar {
  class barbar {
  }
}
"""

    assertErrors(List(columnError(4, 6, List("^[A-Z][A-Za-z]*$")), columnError(5, 8, List("^[A-Z][A-Za-z]*$"))), source)
  }
}

class ObjectNamesCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "object.name"
  val classUnderTest = classOf[ObjectNamesChecker]

  @Test def testZero(): Unit = {
    val source = """
package foobar

object Foobar {
  val foo = 1
}
"""

    assertErrors(List(), source)
  }

  @Test def testOne(): Unit = {
    val source = """
package foobar

object foobar {
  object barbar {
  }
}
"""

    assertErrors(List(columnError(4, 7, List("^[A-Z][A-Za-z]*$")), columnError(5, 9, List("^[A-Z][A-Za-z]*$"))), source)
  }

  @Test def testPackageObject(): Unit = {
    val source = """
package foobar

package object foobar {
  object barbar {
  }
}
"""

    assertErrors(List(columnError(5, 9, List("^[A-Z][A-Za-z]*$"))), source)
  }
}


class PackageNamesCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "package.name"
  val classUnderTest = classOf[PackageNamesChecker]

  @Test def testSinglePartNoError(): Unit = {
    val source = """
package foobar
"""

    assertErrors(List(), source)
  }

  @Test def testSinglePartError(): Unit = {
    val source = """
package FooBar
"""

    assertErrors(List(columnError(2, 8, List("^[a-z][A-Za-z]*$"))), source)
  }

  @Test def testMultiPartNoError(): Unit = {
    val source = """
package abc.foobar
"""

    assertErrors(List(), source)
  }

  @Test def testMultiPartError(): Unit = {
    val source = """
package abc.foo_bar
"""

    assertErrors(List(columnError(2, 12, List("^[a-z][A-Za-z]*$"))), source)
  }

  @Test def testPackageObjectNoError(): Unit = {
    val source = """
package object _foo_bar
"""

    assertErrors(List(), source)
  }

  // Check case where the package name is built up by successive package statements.
  @Test def testMultiLinePackageNoError(): Unit = {
    val source = """
package foo
package bar
"""

    assertErrors(List(), source)
  }

  // Check case where the package name is built up by successive package statements.
  @Test def testMultiLinePackageError(): Unit = {
    val source = """
package foo
package Bar
"""

    assertErrors(List(columnError(3, 8, List("^[a-z][A-Za-z]*$"))), source)
  }

  @Test def testMultiLinePackageMultipleError(): Unit = {
    val source = """
package Foo
package Bar
"""

    assertErrors(List(columnError(2, 8, List("^[a-z][A-Za-z]*$")), columnError(3, 8, List("^[a-z][A-Za-z]*$"))), source)
  }

  @Test def testPackageAndPackageObjectNoError(): Unit = {
    val source = """
package org.thisone
package object _foo
"""

    assertErrors(List(), source)
  }

}

class PackageObjectNamesCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "package.object.name"
  val classUnderTest = classOf[PackageObjectNamesChecker]

  @Test def testZero(): Unit = {
    val source = """
package foobar

package object foobar {
  val foo = 1
}
"""

    assertErrors(List(), source)
  }

  @Test def testOne(): Unit = {
    val source = """
package foobar

package object Foobar {
}
package object Barbar {
}
"""

    assertErrors(List(columnError(4, 15, List("^[a-z][A-Za-z]*$")), columnError(6, 15, List("^[a-z][A-Za-z]*$"))), source)
  }

  @Test def testPackageObject(): Unit = {
    val source = """
package foobar

object foobar {
  object barbar {
  }
}
"""

    assertErrors(List(), source)
  }
}

class MethodNamesCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "method.name"
  val classUnderTest = classOf[MethodNamesChecker]

  @Test def testDefault(): Unit = {
    val source = """
package foobar

class Foobar {
  def foo() = 1
  def +() = 1
//  def foo+() = 1
  def setting_=(s: Boolean) {}
}
"""

    assertErrors(List(defErr(6, 6)), source)
  }

  @Test def testNonDefault(): Unit = {
    val source = """
package foobar

class Foobar {
  def Foo() = 1
  def +() = 1
//  def Foo+() = 1
}
"""

    assertErrors(List(columnError(6, 6, List("^F[o*]*$"))), source, Map("regex" -> "^F[o*]*$"))
  }

  @Test def testWithIgnoreRegex(): Unit = {
    val source = """
package foobar

class Foobar {
  def foo() = 1
  def +() = 1
  def -() = 1
//  def Foo+() = 1
}
"""

    assertErrors(List(defErr(7, 6)), source, Map("ignoreRegex" -> "^\\+$"))
  }

  @Test def testIgnoreOverride(): Unit = {
    val source = """
package foobar

trait Bar {
  def +() = 1
  def -() = 1
  def &() = 1
}

class Foobar extends Bar {
  override def +() = 1
  protected override def &() = 1
  override protected def -() = 1
  def bar() = 1
}
"""

    assertErrors(List(defErr(5, 6), defErr(6, 6), defErr(7, 6)), source, Map("ignoreOverride" -> "true"))
  }

  private def defErr(line: Int, column: Int) = columnError(line, column, List("^[a-z][A-Za-z0-9]*(_=)?$"))
}

class MethodArgumentNamesCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "method.argument.name"
  val classUnderTest = classOf[MethodArgumentNamesChecker]

  @Test
  def testDefaultRegex(): Unit = {
    val defaultRegex = "^[a-z][A-Za-z0-9]*$"

    val source = """
package foobar

class Foobar {
  def foo() = 1
  def +() = 1
//  def foo+() = 1
  def setting(s: Boolean) {}
  def fetch(Query: Boolean) {}
  def multiple(foo: String, BAR: String) {}
}
"""

    assertErrors(List(columnError(9, 6, List(defaultRegex)), columnError(10, 6, List(defaultRegex))), source)
  }

  @Test
  def testProvidedRegex(): Unit = {
    val providedRegex = "^F[o*]*$"

    val source = """
package foobar

class Foobar {
  def valid(Foo: String, Foooooooo: Int) = 1
  def invalid1(foo: String) = 2
  def invalid2(FOO: String) = 3
}
"""

    assertErrors(List(columnError(6, 6, List(providedRegex)), columnError(7, 6, List(providedRegex))), source, Map("regex" -> providedRegex))
  }

  @Test
  def testWithIgnoreRegex(): Unit = {
    val ignoreRegex = "^M[a-zA-Z0-9]*$"
    val defaultRegex = "^[a-z][A-Za-z0-9]*$"

    val source = """
package foobar

class Foobar {
  def ignore(Moo: String) = 1
  def valid(validArg: String) = 1
  def invalidArg(INVALID: String) = 1
}
"""

    assertErrors(List(columnError(7, 6, List(defaultRegex))), source, Map("ignoreRegex" -> ignoreRegex))
  }
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

  @Test def testDestructuring(): Unit = {
    val source = """val (a, b, c) = f()
                   |val Case(a, Case(b, c)) = ff()
                 """.stripMargin
    val badSource = """val (a, B, c) = f()
                      |val Case(Aa, Case(A, B())) = ff()
                    """.stripMargin

    assertErrors(List(), source)
    assertErrors(List(
      columnError(1, 8, List("^[a-z][A-Za-z0-9]*$")),
      columnError(2, 9, List("^[a-z][A-Za-z0-9]*$")),
      columnError(2, 18, List("^[a-z][A-Za-z0-9]*$"))), badSource)
  }

  @Test def testDestructuringWithTypesOK(): Unit = {
    val source =
      """val (foo: Foo, bar: Bar) = baz
        |""".stripMargin

    assertErrors(List(), source)
  }

  @Test def testDestructuringWithTypesKO(): Unit = {
    val source =
      """val (BBB: Foo, AAA: Bar) = baz
        |""".stripMargin

    assertErrors(List(
      columnError(1, 5, List("^[a-z][A-Za-z0-9]*$")),
      columnError(1, 15, List("^[a-z][A-Za-z0-9]*$"))), source)
  }

  @Test def testObjectConst(): Unit = {
    val source = """object O {
                   |  val Name = 1
                   |  class C {
                   |    val name = 1
                   |  }
                   |}
                 """.stripMargin
    val badSource = """object O {
                      |  val name = 1
                      |}
                    """.stripMargin

    assertErrors(List(), source)
    assertErrors(List(columnError(2, 6, List("^[A-Z][A-Za-z0-9]*$"))), badSource)
  }
  
  @Test def testValidConstructorParamsOK(): Unit = {
    val source =
      """
        |class foobar(val myArg1: String, var myArg2: Int)  {
        |}
      """.stripMargin

    assertErrors(List(), source)
  }

  @Test def testInValidConstructorParamsKO(): Unit = {
    val source =
      """
        |class foobar(val MyArg1: String, var MyArg2: Int)  {
        |}
      """.stripMargin

    assertErrors(List(
      columnError(2, 17, List("^[a-z][A-Za-z0-9]*$")),
      columnError(2, 37, List("^[a-z][A-Za-z0-9]*$"))
    ), source)
  }
}
