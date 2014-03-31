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
import org.scalastyle.Checker
import org.scalastyle.file.CheckerTest
import org.scalatest.junit.AssertionsForJUnit

// scalastyle:off magic.number

class SpaceAfterCommentStartCheckerTest extends AssertionsForJUnit with CheckerTest {
  override protected val key: String = "space.after.comment.start"
  override protected val classUnderTest = classOf[SpaceAfterCommentStartChecker]

  @Test def testSinglelineComments(): Unit = {
    val source = """
package foobar

object Foobar {
  //Incorrect
  // correct comment
  /////////////////////////////////
  ///Invalid
  /// Invalid
}"""

    assertErrors(List(columnError(5, 2), columnError(8, 2), columnError(9, 2)), source)
  }

  @Test def testMultipleInlineComments(): Unit = {
    val source = """
package foobar

object Foobar {
  //Incorrect
  // correct comment//not wrong//check
  val a = 10//Incorrect
  val b = 100 //Incorrect
  val c = 1// Correct
  val d = 2 // Correct
  val e = 3
}"""
    assertErrors(List(columnError(5, 2), columnError(7, 12), columnError(8, 14)), source)

  }

  @Test def testMultilineComments(): Unit = {
    val source = """
package foobar

object Foobar {
  /*WRONG
  *
  */
  /* Correct */
  /* Wrong*/
  val d = 2 /*Wrong*/
  /*
   *Correct
   */
  val e = 3/* Correct */
}"""

    assertErrors(List(columnError(5, 2), columnError(9, 2), columnError(10, 12)), source)
  }


  @Test def testScaladocsComments(): Unit = {
    val source = """
package foobar

object Foobar {
  /**WRONG
  *
  */
  /** Correct */
  val d = 2 /**Wrong*/
  /** Wrong*/
  /**
   *Correct
   */
  val e = 3/** Correct */
}"""
    assertErrors(List(columnError(5, 2), columnError(9, 12), columnError(10, 2)), source)
  }

  @Test def testMixedComments(): Unit = {
    val source = """
package foobar

object Foobar {
  /**WRONG
  *
  */
  /** Correct */
  val d = 2 /*Wrong*/ //Wrong
  /**
   *Correct
   */
  val e = 3/** Correct */ // Correct
}"""
    assertErrors(List(columnError(5, 2), columnError(9, 12), columnError(9, 22)), source)
  }
}
