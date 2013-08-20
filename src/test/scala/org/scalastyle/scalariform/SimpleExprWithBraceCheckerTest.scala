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

// scalastyle:off magic.number

package org.scalastyle.scalariform

import org.scalatest.junit.AssertionsForJUnit
import org.scalastyle.file.CheckerTest
import org.junit.Test


class SimpleExprWithBraceCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "simple.expression.with.brace"
  val classUnderTest = classOf[SimpleExprWithBraceChecker]

  @Test def testValOK(): Unit = {
    val code = """
class A{
  val x = 1
}
""".stripMargin
    assertErrors(List(), code, Map("targetTokens" -> "val"))
  }

  @Test def testValWithSingleExprBody(): Unit = {
    val code = """
class A{
  val x = {
    1
  }
}
""".stripMargin
    assertErrors(List(columnError(3, 10)), code, Map("targetTokens" -> "val"))
  }

  @Test def testValWithMultipleExprBody(): Unit = {
    val code = """
class A{
  val x = {
    1
    2
  }
}
""".stripMargin
    assertErrors(List(), code, Map("targetTokens" -> "val"))
  }

  @Test def testVarOK(): Unit = {
    val code = """
class A{
  var x = 1
}
""".stripMargin
    assertErrors(List(), code, Map("targetTokens" -> "var"))
  }

  @Test def testVarWithSingleExprBody(): Unit = {
    val code = """
class A{
  var x = {
   1
  }
}
""".stripMargin
    assertErrors(List(columnError(3, 10)), code, Map("targetTokens" -> "var"))
  }

  @Test def testVarWithMultipleExprBody(): Unit = {
    val code = """
class A{
  var x = {
   1
   2
  }
}
""".stripMargin
    assertErrors(List(), code, Map("targetTokens" -> "var"))
  }

  @Test def testDefOK(): Unit = {
    val code = """
class A{
 def a() = 1
}
""".stripMargin

    assertErrors(List(), code, Map("targetTokens" -> "def"))
  }

  @Test def testDefWithSingleExprBody(): Unit = {
    val code = """
class A{
 def a() = {
  1
 }
}
""".stripMargin

    assertErrors(List(columnError(3, 11)), code, Map("targetTokens" -> "def"))
  }

  @Test def testDefWithMultipleExprBody(): Unit = {
    val code = """
class A{
 def b() = {
  foo()
  bar()
 }
}
""".stripMargin

    assertErrors(List(), code, Map("targetTokens" -> "def"))
  }

  @Test def testForWithSingleExprBody(): Unit = {
    val code = """
class A{
 def a() = {
  for(b <- List(1,2,3)){
   b + 1
  }
 }
}
""".stripMargin
    assertErrors(List(columnError(4, 23)), code, Map("targetTokens" -> "for"))
  }

  @Test def testForWithMultipleExprBody(): Unit = {
    val code = """
class A{
 def a() = {
  for(b <- List(1,2,3)){
   foo()
   bar()
  }
 }
}
""".stripMargin

    assertErrors(List(), code, Map("targetTokens" -> "for"))
  }

  @Test def testForSingleLine(): Unit = {
    val code = """
package foobar
class A{
 def a() = {
  // single line for expr
  for(b <- List(1,2,3)) foo()
 }
}
""".stripMargin

    assertErrors(List(), code, Map("targetTokens" -> "for"))
  }

  @Test def testWhileSingleLineBody(): Unit = {
    val code = """
package foobar
class A{
 def a() = {
  while(true){
   foo()
  }
 }
}
""".stripMargin
    assertErrors(List(columnError(5, 13)), code, Map("targetTokens" -> "while"))
  }

  @Test def testWhileLongBody(): Unit = {
    val code = """
package foobar
class A{
 def a() = {
  while(true){
   foo()
   bar()
  }
 }
}
""".stripMargin

    assertErrors(List(), code, Map("targetTokens" -> "while"))
  }

  @Test def testWhileSingleLine(): Unit = {
    val code = """
package foobar
class A{
 def a() = {
  while(true) 1
 }
}
""".stripMargin

    assertErrors(List(), code, Map("targetTokens" -> "while"))
  }

  @Test def testIfOK(): Unit = {
    val code = """
package foobar
class A{
 def a() = {
  if(true) foo()
 }
}
""".stripMargin

    assertErrors(List(), code, Map("targetTokens" -> "if"))
  }

  @Test def testIfWithSingleExprBody(): Unit = {
    val code = """
class A {
 def a() = {
  if(true){
   foo()
  }
 }
}
""".stripMargin
    assertErrors(List(columnError(4, 10)), code, Map("targetTokens" -> "if"))
  }

  @Test def testIfWithMultipleExprBody(): Unit = {
    val code = """
class A {
 def a() = {
  if(true){
   foo()
   bar()
  }
 }
}
""".stripMargin
    assertErrors(List(), code, Map("targetTokens" -> "if"))
  }

  @Test def testElseOK(): Unit = {
    val code = """
class A {
 def a() = {
  if(true){
   foo()
  }else bar()
 }
}
""".stripMargin
    assertErrors(List(), code, Map("targetTokens" -> "else"))
  }

  @Test def testElseWithSingleExprBody(): Unit = {
    val code = """
class A {
 def a() = {
  if(true){
   foo()
  }else{
   bar()
  }
 }
}
""".stripMargin
    assertErrors(List(columnError(6, 7)), code, Map("targetTokens" -> "else"))
  }

  @Test def testElseWithMultipleExprBody(): Unit = {
    val code = """
class A {
 def a() = {
  if(true){
   foo()
  }else{
   bar()
   bar()
  }
 }
}
""".stripMargin
    assertErrors(List(), code, Map("targetTokens" -> "else"))
  }

  @Test def testCaseWithSingleExprBody(): Unit = {
    val code = """
class A {
 def a() = 1 match {
  case x => Nil
  case y => {
   Nil
  }
 }
}
""".stripMargin
    assertErrors(List(columnError(5, 12)), code, Map("targetTokens" -> "case"))
  }

  @Test def testCaseWithMultipleExprBody(): Unit = {
    val code = """
class A {
 def a() = 1 match {
  case x => Nil
  case y => {
   foo()
   bar()
  }
 }
}
""".stripMargin
    assertErrors(List(), code, Map("targetTokens" -> "case"))
  }

  @Test def testNestedExpressionAllowed(): Unit = {
    val code = """
class A {
  def a() = {
    List(1,2,3).map(_ + 1).filter(_ == 1)
  }
}""".stripMargin
    assertErrors(List(), code, Map("targetTokens" -> "def", "nestedAllowed" -> "true"))
  }

  @Test def testNestedExpressionNotAllowed(): Unit = {
    val code = """
class A {
  def a() = {
    List(1,2,3).map(_ + 1).filter(_ == 1)
  }
}""".stripMargin
    assertErrors(List(columnError(3, 12)), code, Map("targetTokens" -> "def", "nestedAllowed" -> "false"))
  }
}
