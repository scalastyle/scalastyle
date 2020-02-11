package org.scalastyle

import java.io.File

import org.junit.{Assert, Test}
import org.scalatestplus.junit.AssertionsForJUnit

class DirectoryTest extends AssertionsForJUnit {

  private def toFile(s: String) = {
    val base = "src/test/resources/testDir/"
    new File(base + s)
  }

  @Test
  def processSingleFileOnlyOnce(): Unit = {
    val duplicatedFile = List("dirA/X.scala", "dirA/X.scala")
    assertGetFiles(1, duplicatedFile)
  }

  @Test
  def processDirectoryOnlyOnce(): Unit = {
    val duplicatedDir = List("dirA", "dirA")
    assertGetFiles(1, duplicatedDir)
  }

  @Test
  def processFileInProcessDirOnlyOnce(): Unit = {
    val duplicatedFileInDir = List("dirB", "dirB/Y.scala")
    assertGetFiles(2, duplicatedFileInDir)
  }

  private def assertGetFiles(nrExpectedFiles: Int, inputFiles: Seq[String]): Unit = {
    val files = Directory.getFiles(None, inputFiles.map(toFile))
    Assert.assertEquals(nrExpectedFiles, files.size)
  }

}
