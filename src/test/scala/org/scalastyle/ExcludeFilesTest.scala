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

package org.scalastyle

import java.io.File

import org.junit.Assert
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

// scalastyle:off multiple.string.literals

class ExcludeFilesTest extends AssertionsForJUnit {
  private val base = "src/test/resources/testDir/"
  private def toFile(s: String) = new File(base + s)

  @Test
  def specifyExcludedDirectory(): Unit = {
    assertFiles(List("dirB/Y.scala", "dirB/Z.scala"), List(""), List(""".*dirA.*"""))
  }

  @Test
  def specifyExcludedFile(): Unit = {
    assertFiles(List("dirA/X.scala", "dirB/Y.scala"), List(""), List(""".*Z\.scala"""))
  }

  @Test
  def specifyExcludedFileWithMultipleSources(): Unit = {
    assertFiles(List("dirA/X.scala", "dirB/Y.scala"), List("dirA", "dirB"), List(""".*Z\.scala"""))
  }

  @Test
  def specifyMultipleExclusionRules(): Unit = {
    assertFiles(List("dirB/Y.scala"), List("dirA", "dirB"), List(""".*Z\.scala""", """.*X\.scala"""))
  }

  @Test
  def noExclusion(): Unit = {
    assertFiles(List("dirA/X.scala", "dirB/Y.scala", "dirB/Z.scala"), List("dirA", "dirB"), Nil)
  }

  @Test
  def noExclusion2(): Unit = {
    assertFiles(List("dirA/X.scala", "dirB/Y.scala", "dirB/Z.scala"), List(""), Nil)
  }

  private def assertFiles(expected: List[String], files: List[String], exclude: List[String]): Unit = {
    val list = Directory.getFiles(None, files.map(f => toFile(f)), exclude)
    Assert.assertEquals(expected.map(f => toFile(f).getAbsolutePath).sorted, list.map(_.name).sorted)
  }
}
