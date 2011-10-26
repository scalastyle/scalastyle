package org.segl.scalastyle

import java.io.File;

class Main

object Main {
  def main(args: Array[String]): Unit = {
    val configuration = ScalastyleConfiguration.readFromXml("src/main/resources/scalastyle_config.xml")
    
    val messages = new ScalastyleChecker().checkFiles(configuration, Directory.getFiles(new File("src/main/scala")))
    

//    new XmlOutput().output(messages);	
    new TextOutput().output(messages);
  }
}