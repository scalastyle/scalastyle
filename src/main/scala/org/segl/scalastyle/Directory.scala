package org.segl.scalastyle

import java.io._

class Directory

object Directory {
  val scalaFileFilter = new FileFilter() {
      def accept(file: File): Boolean = file.getAbsolutePath().endsWith(".scala")
    }
  
  def getFiles(dir: File): List[String] = {
    dir.listFiles(scalaFileFilter).map(_.getAbsolutePath()).toList ::: dir.listFiles().filter(_.isDirectory).flatMap(getFiles(_)).toList
  }
}