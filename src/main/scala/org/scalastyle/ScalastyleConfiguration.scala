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

package org.scalastyle

import scala.Option.option2Iterable
import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml.Atom
import scala.xml.Comment
import scala.xml.Elem
import scala.xml.EntityRef
import scala.xml.Group
import scala.xml.NamespaceBinding
import scala.xml.Node
import scala.xml.PrettyPrinter
import scala.xml.ProcInstr
import scala.xml.Text
import scala.xml.TextBuffer
import scala.xml.Utility
import scala.xml.XML
import scala.xml.Attribute
import scala.xml.MinimizeMode

object Level {
  val Warning = "warning"
  val Error = "error"
  val Info = "info"

  def apply(s: String): Level = s match {
    case Warning => WarningLevel
    case Error => ErrorLevel
    case Info => InfoLevel
    case _ => WarningLevel
  }
}
sealed abstract class Level(val name: String)
case object ErrorLevel extends Level(Level.Error)
case object WarningLevel extends Level(Level.Warning)
case object InfoLevel extends Level(Level.Info)

object ParameterType {
  val Integer = "integer"
  val String = "string"
  val Boolean = "boolean"

  def apply(s: String): ParameterType = s match {
    case Integer => IntegerType
    case String => StringType
    case Boolean => BooleanType
    case _ => StringType
  }
}
sealed abstract class ParameterType(val name: String)
case object IntegerType extends ParameterType(ParameterType.Integer)
case object StringType extends ParameterType(ParameterType.String)
case object BooleanType extends ParameterType(ParameterType.Boolean)

case class ConfigurationChecker(className: String, level: Level, enabled: Boolean, parameters: Map[String, String],
                                customMessage: Option[String], customId: Option[String])

object ScalastyleConfiguration {
  val DefaultConfiguration: String = "/default_config.xml"
  val Enabled = "enabled"
  val Disabled = "disabled"
  val True = "true"
  val False = "false"
  val Name = "name"

  def getDefaultConfiguration(): ScalastyleConfiguration = {
    val is = this.getClass().getClassLoader().getResourceAsStream(DefaultConfiguration)
    fromXml(XML.load(is))
  }

  def readFromXml(file: String): ScalastyleConfiguration = fromXml(XML.loadFile(file))
  def readFromString(s: String): ScalastyleConfiguration = fromXml(XML.loadString(s))

  private[this] def fromXml(elem: Elem) = {
    val commentFilter = elem.attribute("commentFilter").getOrElse(scala.xml.Text(Enabled)).text.toLowerCase() != Disabled
    val name = (elem \\ Name).text

    ScalastyleConfiguration(name, commentFilter, (elem \\ "check").map(toCheck).toList)
  }

  private def contentsOf(node: Node, n: String) = {
    val ns = (node \\ n)
    if (ns.size == 0) None else (Some(ns(0).text))
  }

  def toCheck(node: Node): ConfigurationChecker = {
    val className = node.attribute("class").get.text
    val level = Level(node.attribute("level").get.text)
    val enabled = node.attribute(Enabled).getOrElse(scala.xml.Text(False)).text.toLowerCase() == True
    val customMessage = contentsOf(node, "customMessage")
    val customId = node.attribute("customId").flatMap(x => Some(x.text))

    ConfigurationChecker(className, level, enabled, (node \\ "parameters" \\ "parameter").map(e => {
      val attributeValue = e.attribute("value")
      val value = if (attributeValue.isDefined) attributeValue.get.text else e.text
      (e.attribute(Name).head.text -> value)
    }).toMap, customMessage, customId)
  }

  private[this] def toCDATA(s: String) = scala.xml.Unparsed("<![CDATA[" + s + "]]>")

  def toXml(scalastyleConfiguration: ScalastyleConfiguration): scala.xml.Elem = {
    val elements = scalastyleConfiguration.checks.map(c => {
      val parameters = if (c.parameters.size > 0) {
        val ps = c.parameters.map(p => {
          val text = toCDATA(p._2)
          <parameter name={p._1}>{text}</parameter>
        })
        <parameters>{ps}</parameters>
      } else {
        scala.xml.Null
      }
      val customMessage = c.customMessage match {
        case Some(s) => {
          val text = toCDATA(s)
          <customMessage>{text}</customMessage>
        }
        case None => scala.xml.Null
      }
      val check = <check class={c.className} level={c.level.name} enabled={if (c.enabled) True else False}>{customMessage}{parameters}</check>
      c.customId match {
        case Some(x) => check % Attribute(None, "customId", Text(x), scala.xml.Null)
        case None => check
      }
    })

    <scalastyle commentFilter={if (scalastyleConfiguration.commentFilter) Enabled else Disabled}>
      <name>{scalastyleConfiguration.name}</name>
      {elements}
    </scalastyle>
  }

  def toXmlString(scalastyleConfiguration: ScalastyleConfiguration, width: Int, step: Int): String =
               new XmlPrettyPrinter(width, step).format(toXml(scalastyleConfiguration))
}

case class ScalastyleConfiguration(name: String, commentFilter: Boolean, checks: List[ConfigurationChecker])

// definition

case class DefinitionParameter(name: String, typeName: ParameterType, multiple: Boolean, defaultValue: String)
case class DefinitionChecker(className: String, id: String, level: Level, parameters: Map[String, DefinitionParameter])

object ScalastyleDefinition {
  def readFromXml(stream: java.io.InputStream): ScalastyleDefinition = {
    val elem = XML.load(stream)

    ScalastyleDefinition((elem \\ "checker").map(toCheck).toList)
  }

  def toCheck(node: Node): DefinitionChecker = {
    val className = stringAttr(node, "class")
    val id = stringAttr(node, "id")
    val defaultLevel = levelAttr(node, "defaultLevel")

    DefinitionChecker(className, id, defaultLevel, (node \\ "parameters" \\ "parameter").map(e => {
      val parameterName = stringAttr(e, "name")
      val parameterType = typeAttr(e, "type")
      val multiple = booleanAttr(e, "multiple")
      val defaultValue = stringAttr(e, "default")
      (parameterName -> DefinitionParameter(parameterName, parameterType, multiple, defaultValue))
    }).toMap)
  }

  def stringAttr(node: Node, id: String): String = {
    attr(node, id, "", {s => s})
  }

  def levelAttr(node: Node, id: String): Level = {
    attr(node, id, Level.Warning, {s => Level(s)})
  }

  def typeAttr(node: Node, id: String): ParameterType = {
    attr(node, id, "string", {s => ParameterType(s)})
  }

  def booleanAttr(node: Node, id: String): Boolean = {
    attr(node, id, "false", {s => "true" == s.toLowerCase()})
  }

  def attr[T](node: Node, id: String, defaultValue: String, fn: (String) => T): T = {
    node.attribute(id) match {
      case Some(x) => fn(x.text)
      case _ => fn(defaultValue)
    }
  }
}

case class ScalastyleDefinition(checkers: List[DefinitionChecker])

// it's unfortunate that we have to do this, but the scala xml PrettyPrinter converts CDATA sections to
// Text, which means that multiple lines get wrapped into one. So we extend PrettyPrinter
// so that they don't get eaten
// see also https://issues.scala-lang.org/browse/SI-3368
class XmlPrettyPrinter(width: Int, step: Int) extends PrettyPrinter(width, step) {

  // this is the method which has changed.
  private def doPreserve(node: Node) = true

  // This is just a copy of what's in scala.xml.PrettyPrinter
    /** @param tail: what we'd like to squeeze in */
  protected override def traverse(node: Node, pscope: NamespaceBinding, ind: Int): Unit =  node match {

      case Text(s) if s.trim() == "" =>
        ;
      case _:Atom[_] | _:Comment | _:EntityRef | _:ProcInstr =>
        makeBox(ind, node.toString().trim() )
      case g @ Group(xs) =>
        traverse(xs.iterator, pscope, ind)
      case _ =>
        val test = {
          val sb = new StringBuilder()
          Utility.serialize(node, pscope, sb, false, minimizeTags = MinimizeMode.Default)
          if (doPreserve(node)) sb.toString else TextBuffer.fromString(sb.toString()).toText(0).data
        }
        if (childrenAreLeaves(node) && fits(test)) {
          makeBox(ind, test)
        } else {
          val (stg, len2) = startTag(node, pscope)
          val etg = endTag(node)
          if (stg.length < width - cur) { // start tag fits
            makeBox(ind, stg)
            makeBreak()
            traverse(node.child.iterator, node.scope, ind + step)
            makeBox(ind, etg)
          } else if (len2 < width - cur) {
            // <start label + attrs + tag + content + end tag
            makeBox(ind, stg.substring(0, len2))
            makeBreak() // todo: break the rest in pieces
            /*{ //@todo
             val sq:Seq[String] = stg.split(" ");
             val it = sq.iterator;
             it.next;
             for (c <- it) {
               makeBox(ind+len2-2, c)
               makeBreak()
             }
             }*/
            makeBox(ind, stg.substring(len2, stg.length))
            makeBreak()
            traverse(node.child.iterator, node.scope, ind + step)
            makeBox(cur, etg)
            makeBreak()
          } else { // give up
            makeBox(ind, test)
            makeBreak()
          }
        }
  }
}
