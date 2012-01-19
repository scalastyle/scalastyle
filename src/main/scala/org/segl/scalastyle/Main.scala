package org.segl.scalastyle

import java.io.File;

class Main

object Main {
  def main(args: Array[String]): Unit = {
    val configuration = ScalastyleConfiguration.readFromXml(args(0))

    val messages = new ScalastyleChecker().checkFiles(configuration, Directory.getFiles(new File(args(1))))

    new TextOutput().output(messages);
  }
}