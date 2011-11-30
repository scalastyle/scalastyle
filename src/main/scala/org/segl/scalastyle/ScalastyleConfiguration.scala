package org.segl.scalastyle

import scala.xml.XML
import scala.xml.Elem;
import scala.xml.Node;

case class ConfigCheck(className: String, parameters: Map[String, String])

object ScalastyleConfiguration {
  def readFromXml(file: String): ScalastyleConfiguration = {
    val elem = XML.loadFile(file)

    val name = (elem \\ "name").text
    println("name=" + name)

    ScalastyleConfiguration(name, (elem \\ "check").map(toCheck).toList)
  }

  def toCheck(node: Node): ConfigCheck = {
    val className = node.attribute("class").head.text

    ConfigCheck(className, (node \\ "parameters" \\ "parameter").map(e => (e.attribute("name").head.text -> e.attribute("value").head.text)).toMap)
  }
}

case class ScalastyleConfiguration(name: String, checks: List[ConfigCheck])
