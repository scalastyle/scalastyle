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

class ScalaDocCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "scaladoc"
  val classUnderTest = classOf[ScalaDocChecker]

  import ScalaDocChecker._ // scalastyle:ignore underscore.import import.grouping

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
        assertErrors(if (checked) List(lineError(1, List(Missing))) else Nil, source format "")
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
    val annotatedCaseClassSource = "%scase class Foo @JpaAbomination() (@Field a: Int, @Field b: Int)"
    val annotatedCaseClassSource2 = """%scase class Foo @JpaAbomination(me) (@Field(a = 4, b = "foo") a: Int, @Field() b: Int)"""
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

    List(classSource, caseClassSource, annotatedCaseClassSource, annotatedCaseClassSource2).foreach { source =>
      assertErrors(Nil, source format doc)
      assertErrors(List(lineError(1, List(Missing))), source format "")
      assertErrors(List(lineError(5, List(missingParam("a"))), lineError(5, List(missingParam("b")))), source format missingParamDoc)
    }
  }

  @Test def specClass(): Unit = {
    val specClassSource = "%sclass FooSpec"
    val specITClassSource = "%sclass FooSpecIT"
    val doc =
      """
        |/**
        | * This is the documentation for whatever follows
        | */
      """.stripMargin

    List(specClassSource, specITClassSource).foreach { source =>
      assertErrors(List(lineError(1, List(Missing))), source format "")
      assertErrors(Nil, source format doc)
      assertErrors(Nil, source format "", Map("ignoreRegex" -> "(.+Spec$)|(.+SpecIT$)"))
      assertErrors(Nil, source format doc, Map("ignoreRegex" -> "(.+Spec$)|(.+SpecIT$)"))
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
      assertErrors(List(lineError(1, List(Missing))), source format "")
      assertErrors(List(lineError(5, List(MalformedTypeParams))), source format malformedDoc)
    }
  }

  @Test def typeParamsWithBounds(): Unit = {
    val source = "class Foo[@foo A <: B[C], @foo(b) B <: C, C <% D, D: E with A]"
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
        | * @tparam C the type C
        | * @tparam D the type D
        | */
      """.stripMargin

    assertErrors(Nil, doc + source)
    assertErrors(List(lineError(1, List(Missing))), source)
    assertErrors(List(lineError(5, List(MalformedTypeParams))), malformedDoc + source)
  }

  @Test def publicMethodWithEverything(): Unit = {
    def al(access: String = "", checked: Boolean): Unit = {
      val fun =
        s"""
          |/**
          | * XXX
          | */
          |trait X {
          |  %s${access} def foo[A, B, U](a: A, b: B): U = ???
          |}
        """.stripMargin
      val annotatedFun =
        s"""
          |/**
          | * XXX
          | */
          |trait X {
          |  %s${access} def foo[@unchecked A, @annotated B, U](@Field() a: A, @Field("b") b: B): U = ???
          |}
        """.stripMargin
      val proc1 =
        s"""
          |/**
          | * XXX
          | */
          |trait X {
          |  %s${access} def foo[A, B, U](a: A, b: B): Unit = ()
          |}
        """.stripMargin
      val proc2 =
        s"""
          |/**
          | * XXX
          | */
          |trait X {
          |  %s${access} def foo[A, B, U](a: A, b: B) {}
          |}
        """.stripMargin
      def doc(proc: Boolean) =
        """
          |/**
          | * Does foo
          | * @param a the A
          | * @param b the B
          | * @tparam A the A
          | * @tparam B the B
          | * @tparam U the U%s
          | */
        """.stripMargin format (if (proc) "" else "\n * @return some u")

      def missingTypeParamsDoc(proc: Boolean) =
        """
          |/**
          | * Does foo
          | * @param a the A
          | * @param b the B
          | * @tparam A the A
          | * @tparam U the U%s
          | */
          | """.stripMargin format (if (proc) "" else "\n * @return some u")

      def missingParamsDoc(proc: Boolean) =
        """
          |/**
          | * Does foo
          | * @param a the A
          | * @tparam A the A
          | * @tparam B the B
          | * @tparam U the U%s
          | */
          | """.stripMargin format (if (proc) "" else "\n * @return some u")

      val missingReturnDoc =
        """
          |/**
          | * Does foo
          | * @param a the A
          | * @param b the b
          | * @tparam A the A
          | * @tparam B the B
          | * @tparam U the U
          | */
          | """.stripMargin

      List(fun, annotatedFun).foreach { source =>
        assertErrors(Nil, source format doc(false))
        assertErrors(if (checked) List(lineError(6, List(Missing))) else Nil, source format "")
        assertErrors(if (checked) List(lineError(15, List(missingParam("b")))) else Nil, source format missingParamsDoc(false))
        assertErrors(if (checked) List(lineError(15, List(MalformedTypeParams))) else Nil, source format missingTypeParamsDoc(false))
        assertErrors(if (checked) List(lineError(15, List(MalformedReturn))) else Nil, source format missingReturnDoc)
      }

      List(proc1, proc2).foreach { source =>
        assertErrors(Nil, source format doc(false))
        assertErrors(if (checked) List(lineError(6, List(Missing))) else Nil, source format "")
        assertErrors(if (checked) List(lineError(14, List(missingParam("b")))) else Nil, source format missingParamsDoc(true))
        assertErrors(if (checked) List(lineError(14, List(MalformedTypeParams))) else Nil, source format missingTypeParamsDoc(true))
      }
    }

    List("", "final ").foreach { modifier =>
      al(modifier, true)
      al(s"private[xxx] $modifier", true)
      al(s"protected[xxx] $modifier", true)
      al(s"protected $modifier", true)

      al(s"private $modifier", false)

      al(s"@tailrec @another(a = b) $modifier", true)
      al(s"@tailrec @another(a = b) private[xxx] $modifier", true)
      al(s"@tailrec @another(a = b) protected[xxx] $modifier", true)
      al(s"@tailrec @another(a = b) protected $modifier", true)

      al(s"@tailrec @another(a = b) private $modifier", false)
    }
  }

  @Test def returnAsParamDescription(): Unit = {
    val source =
      """
        |/**
        | * Doc
        | */
        |object X {
        |
        |  /**
        |   * Foo does some foos. With a
        |   *
        |   * ```
        |   *     code example here
        |   * ```
        |   * and something or other else with ``code`` and (link)[to]
        |   *
        |   * @param a
        |   *   Some text for parameter A
        |   *   More for A
        |   * @param b B
        |   * @param c
        |   * @return some integer
        |   */
        |  def foo(a: Int, b: Int, c: Int): Int = a + b
        |}
      """.stripMargin

    assertErrors(List(lineError(22, List(emptyParam("c")))), source)
  }

  @Test def valsVarsAndTypes(): Unit = {
    def al(what: String = "", checked: Boolean): Unit = {
      val tlDoc =
        """
          |/**
          | * Top-level doc
          | */
        """.stripMargin
      def source(container: String) =
        s"""
           |$tlDoc
           |$container Foo {
           |  %s${what}
           |}
        """.stripMargin
      val doc =
        """
          |/**
          | * This is the documentation for whatever follows with no params, no tparams, no return, no throws
          | */
        """.stripMargin

      List(source("class"), source("case class"), source("object ")).foreach { source =>
        assertErrors(Nil, source format doc)
        assertErrors(if (checked) List(lineError(8, List(Missing))) else Nil, source format "")
      }
    }

    List("val a = 1", "var a = 2", "type X = String").foreach { member =>
      al(member, true)
      al(s"private[pkg] $member", true)
      al(s"protected[pkg] $member", true)
      al(s"protected $member", true)

      al(s"private $member", false)
    }
  }

  @Test def objectAfterPackage(): Unit = {
    val source =
      """
        |package org.example.test
        |
        |/**
        | * Doc
        | */
        |object X {
        |
        |}
      """.stripMargin

    assertErrors(Nil, source)
  }

  @Test def severalObjects(): Unit = {
    val source =
      """
        |/**
        | * Doc X
        | */
        |object X
        |
        |/**
        | * Doc Y
        | */
        |object Y""".stripMargin

    assertErrors(Nil, source)
  }

  @Test def secondObjectMissingDoc(): Unit = {
    val source =
      """
        |/**
        | * Doc X
        | */
        |object X
        |
        |object Y""".stripMargin

    assertErrors(List(lineError(7, List(Missing))), source)
  }

  @Test def nestedObjectMissingDoc(): Unit = {
    val source =
      """
        |/**
        | * Doc X
        | */
        |object X {
        |
        |  object Y
        |}""".stripMargin

    assertErrors(List(lineError(7, List(Missing))), source)
  }

  @Test def ignoreTokenTypes(): Unit = {

    val cases = Seq(Seq("val a = 1", "var a = 2") -> "PatDefOrDcl",
      Seq("class A", "case class A", "object A", "trait A") -> "TmplDef",
      Seq("type B = A") -> "TypeDefOrDcl",
      Seq("def A(): Unit") -> "FunDefOrDcl")

    for ((declerations, ignoreTokenType) <- cases;
         decleration <- declerations) {
      assertErrors(Nil, decleration, Map("ignoreTokenTypes" -> ignoreTokenType))
    }
  }

  @Test def ignoreOverridden(): Unit = {
    val source =
      """/***/
        |class c {
        |  override def f() = {}
        |  override val v = 1
        |}""".stripMargin

    assertErrors(List.empty, source,
      Map("ignoreOverride" -> "true"))
    assertErrors(List(lineError(3, List(Missing)), lineError(4, List(Missing))), source,
      Map("ignoreOverride" -> "false"))
  }


}
