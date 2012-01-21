package org.segl.scalastyle

import scala.xml.XML
import scala.xml.Elem;
import scala.xml.Node;

sealed abstract class Level(val name: String)
case object ErrorLevel extends Level("error")
case object WarningLevel extends Level("warning")

case class ConfigCheck(className: String, level: Level, parameters: Map[String, String])

object ScalastyleConfiguration {
  def readFromXml(file: String): ScalastyleConfiguration = {
    val elem = XML.loadFile(file)

    val name = (elem \\ "name").text

    ScalastyleConfiguration(name, (elem \\ "check").map(toCheck).toList)
  }

  def toCheck(node: Node): ConfigCheck = {
    val className = node.attribute("class").head.text
    val level = node.attribute("level").head.text match {
      case "warning" => WarningLevel
      case "error" => ErrorLevel
      case _ => WarningLevel
    }

    ConfigCheck(className, level, (node \\ "parameters" \\ "parameter").map(e => (e.attribute("name").head.text -> e.attribute("value").head.text)).toMap)
  }
}

case class ScalastyleConfiguration(name: String, checks: List[ConfigCheck])
