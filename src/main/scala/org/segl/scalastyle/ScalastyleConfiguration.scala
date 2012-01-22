package org.segl.scalastyle

import scala.xml.XML
import scala.xml.Elem;
import scala.xml.Node;

object Level {
  def apply(s: String) = s match {
    case "warning" => WarningLevel
    case "error" => ErrorLevel
    case _ => WarningLevel
  }
}
sealed abstract class Level(val name: String)
case object ErrorLevel extends Level("error")
case object WarningLevel extends Level("warning")

case class ConfigurationChecker(className: String, level: Level, parameters: Map[String, String])

object ScalastyleConfiguration {
  def readFromXml(file: String): ScalastyleConfiguration = {
    val elem = XML.loadFile(file)

    val name = (elem \\ "name").text

    ScalastyleConfiguration(name, (elem \\ "check").map(toCheck).toList)
  }

  def toCheck(node: Node): ConfigurationChecker = {
    val className = node.attribute("class").head.text
    val level = Level(node.attribute("level").head.text)

    ConfigurationChecker(className, level, (node \\ "parameters" \\ "parameter").map(e => {
      (e.attribute("name").head.text -> e.attribute("value").head.text)
    }).toMap)
  }
}

case class ScalastyleConfiguration(name: String, checks: List[ConfigurationChecker])

// definition

case class DefinitionParameter(name: String, typeName: String, defaultValue: String)
case class DefinitionChecker(className: String, id: String, level: Level, parameters: Map[String, DefinitionParameter])

object ScalastyleDefinition {
  def readFromXml(stream: java.io.InputStream): ScalastyleDefinition = {
    val elem = XML.load(stream)

    ScalastyleDefinition((elem \\ "checker").map(toCheck).toList)
  }

  def toCheck(node: Node): DefinitionChecker = {
    val className = node.attribute("class").head.text
    val id = node.attribute("id").head.text
    val defaultLevel = Level(node.attribute("defaultLevel").head.text)

    DefinitionChecker(className, id, defaultLevel, (node \\ "parameters" \\ "parameter").map(e => {
      val parameterName = e.attribute("name").head.text
      val typeName = e.attribute("type").head.text
      val defaultValue = e.attribute("default").head.text
      (e.attribute("name").head.text -> DefinitionParameter(parameterName, typeName, defaultValue))
    }).toMap)
  }
}

case class ScalastyleDefinition(checkers: List[DefinitionChecker])

