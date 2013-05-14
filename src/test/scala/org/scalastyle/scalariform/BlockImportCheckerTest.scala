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

// scalastyle:off magic.number

class BlockImportCheckerTest extends AssertionsForJUnit with CheckerTest {

  val key = "block.import.checker"
  val classUnderTest = classOf[BlockImportChecker]

  @Test
  def singleImportIsNoBlockImport() {
    val source = """
import scala.collection.mutable.Buffer
      """
    assertErrors(Nil, source)
  }

  @Test
  def importAllIsNoBlockImport() {
    val source = """
import scala.collection.mutable._
      """
    assertErrors(Nil, source)
  }

  @Test
  def hideImportIsNoBlockImport() {
    val source = """
import scala.collection.mutable.{Buffer => _}
      """
    assertErrors(Nil, source)
  }

  @Test
  def renameImportIsNoBlockImport() {
    val source = """
import scala.collection.mutable.{Buffer => MB}
      """
    assertErrors(Nil, source)
  }

  @Test
  def commaSeparatedImportIsBlockImport() {
    val source = """
import scala.collection.mutable, mutable.Buffer, mutable.ArrayBuffer
      """
    assertErrors(List(columnError(2, 7)), source)
  }

  @Test
  def blockImportFound() {
    val source = """
import scala.collection.mutable.{Buffer, ArrayBuffer}
      """
    assertErrors(List(columnError(2, 7)), source)
  }
}
