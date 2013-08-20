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

// scalastyle:off magic.number regex

class RemovableMatchCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "removable.match"
  val classUnderTest = classOf[RemovableMatchChecker]

  @Test def testClassOK() {
    val source = """
package foobar

class OK {
  def foo() {
    val l = List(1, 2, 3)
    l.map{
      case 1 => 1
      case _ => 0
    }
    l.map{ i =>
      i match {
        case 1 => i
        case _ => 0
      }
    }
    l.map{ i =>
      l.sliding(2) match {
        case List(x, y) => x + y
      }
    }
    l.foreach{}
    l.foreach(println)
  }
}
                 """;

    assertErrors(List(), source)
  }

  @Test def testClassKO() {
    val source = """
package foobar

class KO {
  def foo() {
    val l = List(1, 2, 3)
    l map{ i => i match {
      case 1 => println("foo")
      case _ =>
      }
    }
    l.map{
      case 1 => println(1)
      case 2 => List(1, 2, 3).map( j => j match { case _ => print(1) } )
      case _ => List(1, 2, 3).map{
        j => j match {
          case _ => List(1, 2, 3).map( k => k match { case _ => print(j) } )
        }
      }
    }
  }
}
                 """;

    assertErrors(List(columnError(7, 6), columnError(14, 30), columnError(17, 34)), source)
  }

  @Test def testVariableHasSameNameInTargetCalls() = {
    val source = """
package foobar

class Foo {
  val count = 1
  val bar = {
    count + count
  }
}
                 """;

    assertErrors(List(), source)
  }
}
