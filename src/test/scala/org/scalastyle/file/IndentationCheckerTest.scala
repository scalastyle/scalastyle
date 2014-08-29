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

package org.scalastyle.file

import org.junit.Test;
import org.scalatest.junit.AssertionsForJUnit

// scalastyle:off magic.number

class IndentationCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "indentation"
  val classUnderTest = classOf[IndentationChecker]

  val cleanSource =
"""
/**
 * Scaladoc comments should pass
 */
class A {
  val foo = 1
  def bar(a: String) = {
    a.length
  }
}

class B(
    paramDoubleIndent: Boolean,
    isAlsoOk: Boolean)
  extends A
{
  override val foo = 2
  val foobar =
    Seq(
      "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
      "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB",
      "CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC"
    )
}
"""
  @Test def testNoErrorsDefaultTabSize(): Unit = {
    assertErrors(List(), cleanSource)
  }

  @Test def forExpression(): Unit = {
    val source =
"""
val id = 50
for {
  a <- findA(id)
  b =  a.b
  c <- b.cs
  if c.isEven
} yield c
"""

    assertErrors(Nil, source)
  }

  @Test def dsl(): Unit = {
    val source =
"""
  |val id = 123
  |val (m, g) = (GroupMember.syntax("m"), Group.syntax("g"))
  |val groupMember = withSQL {
  |  select
  |    .from(GroupMember as m)
  |    .leftJoin(Group as g)
  |    .on(m.groupId, g.id)
  |    .where.eq(m.id, id)
  |}.map(GroupMember(m, g)).single.apply()
""".stripMargin

    assertErrors(Nil, source)
  }


  @Test def testNoErrorsSetTabSize(): Unit = {
    val source = cleanSource replaceAll("  ", "    ")
    assertErrors(List(), source, Map("tabSize" -> "4"))
  }

  @Test def testNoErrorsWithTabs(): Unit = {
    val source = cleanSource replaceAll("  ", "\t")
    assertErrors(List(), source)
  }

  @Test def testErrorsIncorrectTabSize(): Unit = {
    val source =
"""
class A {
 val foo = 1
   def bar(a: String) = {
    a.length
  }
}
"""
    assertErrors(List(lineError(3), lineError(4)), source)
  }

  @Test def testExtraIndent(): Unit = {
    val source =
"""
class A {
  val foo = 1
  def bar(a: String) = {
      a.length
  }
}
"""
    assertErrors(List(lineError(5)), source)
  }
}
