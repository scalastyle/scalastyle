package org.segl.scalastyle

import java.io._
import scala.xml._

class Directory

object Directory {
  val scalaFileFilter = new FileFilter() {
    def accept(file: File): Boolean = file.getAbsolutePath().endsWith(".scala")
  }

  def getFiles(dir: File): List[String] = {
    dir.listFiles(scalaFileFilter).map(_.getAbsolutePath()).toList ::: dir.listFiles().filter(_.isDirectory).flatMap(getFiles(_)).toList
  }

  def main(args: Array[String]): Unit = {
    class Foo(bar: String, bar2: Object) {
    }

    toXml(new Foo("string", "string").getClass)
  }

  def toXml(c: Class[_]) = {
    for (field <- this.getClass.getDeclaredFields)
      "field name=" + field.getName + " tpe=" + field.getType.toString() + this.getClass.getMethods.find(_.getName() == field.getName).get.invoke(this)

  }
}

