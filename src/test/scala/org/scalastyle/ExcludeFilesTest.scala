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

import org.junit.Assert._
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

class ExcludeFilesTest extends AssertionsForJUnit{
  val XFilePath = new File("src/test/resources/testDir/dirA/X.scala").getAbsolutePath
  val YFilePath = new File("src/test/resources/testDir/dirB/Y.scala").getAbsolutePath
  val ZFilePath = new File("src/test/resources/testDir/dirB/Z.scala").getAbsolutePath
  val DirA = new File("src/test/resources/testDir/dirA")
  val DirB = new File("src/test/resources/testDir/dirB")
  val TestDir = new File("src/test/resources/testDir")

  @Test
  def specifyExcludedDirectory(): Unit = {
    val files = Directory.getFiles(None, List(TestDir), Option(""".*dirA.*"""))
    assertEquals(files.map(_.name).toSet, Set(YFilePath, ZFilePath))
  }

  @Test
  def specifyExcludedFile(): Unit = {
    val files = Directory.getFiles(None, List(TestDir), Option(""".*Z\.scala"""))
    assertEquals(files.map(_.name).toSet, Set(XFilePath, YFilePath))
  }

  @Test
  def specifyExcludedFileWithMultipleSources(): Unit = {
    val files = Directory.getFiles(None, List(DirA, DirB), Option(""".*Z\.scala"""))
    assertEquals(files.map(_.name).toSet, Set(XFilePath, YFilePath))
  }

  @Test
  def specifyMultipleExclusionRules(): Unit = {
    val files = Directory.getFiles(None, List(DirA, DirB), Option(""".*Z\.scala;.*X\.scala"""))
    assertEquals(files.map(_.name).toSet, Set(YFilePath))
  }

}
