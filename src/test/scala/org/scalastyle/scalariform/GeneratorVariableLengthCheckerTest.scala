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

class GeneratorVariableLengthCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "generator.variable.length"
  val classUnderTest = classOf[GeneratorVariableLengthChecker]
  val maxLength = List("1")

  @Test def testOK() {
    val source = """
package foobar

class OK {
  def foobar = {
    for(i <- 0 until 10) yield {
      i
    }
    for(i <- 0 until 10; j <- 10 until 20; k <- 20 until 30) yield {
      i * j * k
    }
    for((i, j) <- (0 until 10, 10 until 20)) yield {
      for((k, l) <- (0 until 10, 10 until 20)) {
        long * name
      }
    }
    val l = List( ((1, 2), 3), ((4, 5), 6), ((7, 8), 9))
    for(((x, y), z) <- l) yield {
      x * y * z
    }
    for (List(l, m, r) <- (0 to 12).sliding(3)) yield {
      l * m * r
    }
  }
}
                 """;

    assertErrors(List(), source)
  }

  @Test def testKO() {
    val source = """
package foobar

class KO {
  def foobar = {
    for(foo <- 0 until 10) yield {
      foo
    }
    for(foo <- 0 until 10; i <- 10 until 20; bar <- 20 until 30) yield {
      foo * i * bar
    }
    for((foo, bar) <- (0 until 10, 10 until 20)) yield {
      for((long, name) <- (0 until 10, 10 until 20)) yield {
        long * name
      }
    }
    val l = List( ((1, 2), 3), ((4, 5), 6), ((7, 8), 9))
    for(((foo, bar), baz) <- l) yield {
      foo * bar * baz
    }
    for (List(left, middle, right) <- (0 to 12).sliding(3)) yield {
      left * middle * right
    }
  }
}
                 """;

    assertErrors(List(columnError(6, 8, maxLength), columnError(9, 8, maxLength), columnError(9, 45, maxLength),
      columnError(12, 9, maxLength), columnError(12, 14, maxLength), columnError(13, 11, maxLength),
      columnError(13, 17, maxLength), columnError(18, 10, maxLength), columnError(18, 15, maxLength),
      columnError(18, 21, maxLength), columnError(21, 14, maxLength), columnError(21, 20, maxLength),
      columnError(21, 28, maxLength)), source)
  }
}
