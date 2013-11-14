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

// scalastyle:off magic.number

class ScalaDocCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "scaladoc"
  val classUnderTest = classOf[ScalaDocChecker]

  @Test def noParamsCCTO(): Unit = {
    def al(access: String = "", checked: Boolean): Unit = {
      val traitSource = s"%s${access}trait Foo"
      val classSource = s"%s${access}class Foo"
      val caseClassSource = s"%s${access}case class Foo()"
      val objectSource = s"%s${access}object Foo"
      val doc =
        """
          |/**
          | * This is the documentation for whatever follows with no params, no tparams, no return, no throws
          | */
        """.stripMargin

      List(traitSource, classSource, caseClassSource, objectSource).foreach { source =>
        assertErrors(Nil, source format doc)
        assertErrors(if (checked) List(lineError(1, List("missing"))) else Nil, source format "")
      }
    }

    al("", true)
    al("private[pkg] ", true)
    al("protected[pkg] ", true)
    al("protected ", true)

    al("private ", false)
  }

  @Test def classParams(): Unit = {
    val classSource = "%sclass Foo(a: Int, b: Int)"
    val caseClassSource = "%scase class Foo(a: Int, b: Int)"
    val missingParamDoc =
      """
        |/**
        | * This is the documentation for whatever follows
        | */
      """.stripMargin
    val doc =
      """
        |/**
        | * This is the documentation for whatever follows
        | *
        | * @param a the value of a
        | * @param b the value of b
        | */
      """.stripMargin

    List(classSource, caseClassSource).foreach { source =>
      assertErrors(Nil, source format doc)
      assertErrors(List(lineError(1, List("missing"))), source format "")
      assertErrors(List(lineError(5, List("malformedParams"))), source format missingParamDoc)
    }
  }

  @Test def typeParamsCCT(): Unit = {
    val traitSource = "%strait Foo[A, B]"
    val classSource = "%sclass Foo[A, B]"
    val caseClassSource = "%scase class Foo[A, B]()"
    val malformedDoc =
      """
        |/**
        | * This is the documentation for whatever follows
        | */
      """.stripMargin
    val doc =
      """
        |/**
        | * This is the documentation for whatever follows with tparams
        | *
        | * @tparam A the type A
        | * @tparam B the type B
        | */
      """.stripMargin

    List(traitSource, classSource, caseClassSource).foreach { source =>
      assertErrors(Nil, source format doc)
      assertErrors(List(lineError(1, List("missing"))), source format "")
      assertErrors(List(lineError(5, List("malformedTypeParams"))), source format malformedDoc)
    }
  }

  @Test def methods(): Unit = {

  }
}

/*
package foobar  // missing ScalaDoc is OK on package

trait Foo // missing ScalaDoc :(
class Bar // missing ScalaDoc :(
case class Baz  // missing ScalaDoc :(

private[foobar] trait Foo // missing ScalaDoc OK on non-public
private class Bar         // missing ScalaDoc OK on non-public
private case class Baz    // missing ScalaDoc OK on non-public

/**
 * Proper scaladoc
 */
class A {
  private val ok: Unit = ()

  /**
   * Existence only
   */
  protected val ok1: Unit = ()

  /**
   * Existence only
   */
  val ok2: Unit = ()

  /**
   * With Scaladoc, ignoring its structure.
   *
   * @return ()
   */
  def ok3: Unit = ()

  /**
   * With Scaladoc, ignoring its structure.
   *
   * @return ()
   */
  protected def ok4: Unit = ()

  private def nonPublicOk(): Unit = ()

// the non-OK scenarios

  val notOk1: Unit = ()
  protected val notOk2: Unit = ()
  def notOk3: Unit = ()
  protected def notOk4: Unit = ()

}

 */