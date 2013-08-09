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
import org.scalastyle.file.CheckerTest
import org.junit.Test

// scalastyle:off magic.number multiple.string.literals

class BlankLineBetweenDefinitionCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "blankline.between.definition"
  val classUnderTest = classOf[BlankLineBetweenDefinitionChecker]

  @Test def testMethodDefinedAfterEmptyLine(): Unit = {
    val code = """
package foobar
object Foo
{

  def bar() = Nil;
}
               """.stripMargin

    assertErrors(List(), code)
  }

  @Test def testMethodDefDefinedAfterValidCode(): Unit = {
    val code = """
package foobar
object Foo{
  val foo = 1
  def bar() = Nil
}
               """.stripMargin

    assertErrors(List(columnError(5, 2)), code)

  }

  @Test def testMethodDefinedWithSemiColon(): Unit = {
    val code = """
package foobar
object Foo{

  def bar() = Nil; def foo() = Nil
}
               """.stripMargin

    assertErrors(List(), code)
  }

  @Test def testMethodDefinedAfterEmptyLineWithSingleComment(): Unit = {
    val code = """
package foobar
object Foo{

  // single-line comment
  def bar() = Nil
}
               """.stripMargin

    assertErrors(List(), code)
  }

  @Test def testMethodDefinedAfterEmptyLineWithBlockComment(): Unit = {
    val code = """
package foobar
object Foo{

  /*
    block comment
  */
  def bar() = Nil
}
               """.stripMargin

    assertErrors(List(), code)
  }

  @Test def testNestedComments(): Unit = {
    val code = """
package foobar
object Foo{
  /*

      // nested
    /*

    /*
      // nested
    */
    /*  */
  */ */ /* */
  /*   */
  def bar() = Nil
}
               """.stripMargin

    assertErrors(List(columnError(15, 2)), code)
  }

  @Test def testMethodDefinedAfterNonsenseComments(): Unit = {
    val code = """
package foobar
object Foo{

  /*

    block comment

  */
  def bar() = Nil

  /*
    block comment
  */ def foo() = Nil

  /* block comment */ def foobar() = Nil
  /* block comment */ def barfoo() = Nil
}
               """.stripMargin

    assertErrors(List(columnError(17, 22)), code)

  }

  @Test def testMethodDefinedAfterValidCodeWithSingleComment(): Unit = {
    val code = """
package foobar
object Foo{
  val foo = 1
  // single-line comment
  def bar() = Nil
}
               """.stripMargin

    assertErrors(List(columnError(6, 2)), code)
  }

  @Test def testMethodDefinedAfterValidCodeWithBlockComment(): Unit = {
    val code = """
package foobar
object Foo{
  val foo = 1
  /*
    block comment
  */
  def bar() = Nil
}
               """.stripMargin

    assertErrors(List(columnError(8, 2)), code)

  }

  @Test def testMethodDefinedInInnerClass(): Unit = {
    val code = """
package foobar
object Foo{

  class Bar{

    def foobar() = Nil
  }
}
               """.stripMargin

    assertErrors(List(), code)
  }

  @Test def testMethodDefinedInInnerClassAfterValidCode(): Unit = {
    val code = """
package foobar
object Foo{

  object Bar{
    def foobar() = Nil
  }
}
               """.stripMargin

    assertErrors(List(columnError(6, 4)), code)
  }

  @Test def testMultipleClass(): Unit = {
    val code = """
package foobar
object Foo{

  def foobar() = Nil
}

class Bar {
  def foobar() = Nil
}
               """.stripMargin
    assertErrors(List(columnError(9, 2)), code)
  }

  @Test def testComplicatedClassDefinition(): Unit = {
    val code = """
package foobar
object Foo{

  def foobar() = Nil

  class A {

    class B {
      // bbbb
      def b() = Nil

      object C {

        def c() = Nil

        class D {
          def d() = Nil
        }
      }
    }
  }
}
               """.stripMargin
    assertErrors(List(columnError(11, 6), columnError(18, 10)), code)
  }

  @Test def testAllowNoBlankAfterClass(): Unit = {
    val code = """
package foobar
class Foo {
  def bar() = Nil
}
               """.stripMargin

    assertErrors(List(), code, Map("NoBlankAfterClassAllowed" -> "true"))
  }
}
