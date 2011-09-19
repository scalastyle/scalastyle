package org.segl.scalastyle

import java.io.File;

class Main

class MyListener extends Listener {
  override def start(): Unit = { println("start") }
  override def end(): Unit = { println("end") }

  override def fileStart(file: String): Unit = { println("start file=" + file) }
  override def fileEnd(file: String): Unit = { println("end file=" + file) }

  override def error(file: String, key: String, lineNumber: Int, column: Int) = { println("found error file=" + file + " key=" + key + " lineNumber=" + lineNumber + " column=" + column) }
}

object Main {
  def main(args: Array[String]): Unit = {
    var errors = new ScalastyleChecker().checkFiles(new MyListener(), Directory.getFiles(new File("src/main/scala")))

    errors.foreach(e => println("error=" + e))
  }
}