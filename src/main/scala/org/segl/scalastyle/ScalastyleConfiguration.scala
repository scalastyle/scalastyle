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
    val className = node.attribute("class").get.text
    val level = Level(node.attribute("level").get.text)

    ConfigurationChecker(className, level, (node \\ "parameters" \\ "parameter").map(e => {
      val attributeValue = e.attribute("value")
      val value = if (attributeValue.isDefined) attributeValue.get.text else e.text
      (e.attribute("name").head.text -> value)
    }).toMap)
  }

  def toXml(scalastyleConfiguration: ScalastyleConfiguration): scala.xml.Elem = {
    val elements = scalastyleConfiguration.checks.map(c => {
      val parameters = if (c.parameters.size > 0) {
        val ps = c.parameters.map( p => <parameter name={p._1} value={p._2} />)
        <parameters>{ps}</parameters>
      } else {
        scala.xml.Null
      }
      <check class={c.className} level={c.level.name}>{parameters}</check>
    })

    <scalastyle><name>{scalastyleConfiguration.name}</name>{elements}</scalastyle>
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