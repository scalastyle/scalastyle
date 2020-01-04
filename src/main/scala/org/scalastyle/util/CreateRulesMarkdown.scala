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

package org.scalastyle.util

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths

import scala.xml.Elem
import scala.xml.NodeSeq
import scala.xml.PrettyPrinter
import scala.xml.XML

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import org.scalastyle.BuildInfo
import org.scalastyle.DefinitionChecker
import org.scalastyle.ScalastyleDefinition
import org.scalastyle.XmlPrettyPrinter

// scalastyle:off magic.number

object CreateRulesMarkdown {
  case class CreateRulesMarkdownConfig(file: Option[String])

  // scalastyle:off regex
  private def usage(version: String) = {
    println("scalastyle " + version)
    println("Usage: CreateRulesMarkdown <output file>")

    System.exit(1)
  }
  // scalastyle:on regex

  private val docFile = "scalastyle_documentation.xml"
  private val defnFile = "scalastyle_definition.xml"

  case class Documentation(
    justification: Option[String],
    extraDescription: Option[String],
    example: Seq[String]
  )

  private def toText(elem: NodeSeq) = if (elem.isEmpty) None else Some(elem.text.trim)
  private def toList(elem: NodeSeq) = elem.map(_.text.trim)

  private def toXml(description: String, s: String): Elem = {
    try {
      XML.loadString(s)
    } catch {
      case e: Exception => throw new IllegalArgumentException(description + " for " + s, e)
    }
  }

  private def withTitle(os: Option[String], title: String): List[String] = os match {
    case Some(x) => List("#### " + title, x, "")
    case _       => Nil
  }

  private def id(s: String) = s.replaceAll("\\.", "_")

  private def contents(checkers: List[DefinitionChecker], config: Config): List[String] = {
    val cs = checkers.map { c =>
      val desc = config.getString(c.id + ".description").replaceAll("''\\[''", "'\\\\['")
      "|[" + c.className + "](#" + id(c.className) + ")|" + desc + "|"
    }

    List("""| Checker | Description |""", """| ------------- | ------------- |""") ::: cs ::: List("")
  }

  private def checker(c: DefinitionChecker, doc: Documentation, config: Config): List[String] = {
    val desc = config.getString(c.id + ".description").replaceAll("''\\[''", "'\\\\['")

    val header = List(
      s"""<a name="${id(c.className)}" />""",
      "",
      s"""### ${c.className} - ${desc}""",
      "",
      " * id - " + c.id,
      " * description - " + desc,
      " * class - " + c.className,
      " * default level - " + c.level,
      ""
    )

    val justifcation = withTitle(doc.justification, "Justification")
    val description = withTitle(doc.extraDescription, "Description")

    val parametersTitle = List("#### Parameters")

    val parameters = if (c.parameters.isEmpty) {
      List("No parameters")
    } else {
      val headers = List("Parameter", "Description", "Type", "Default Value").map(s => <th>{s}</th>)
      val header = <tr>{headers}</tr>
      val f = c.parameters.map(p => <tr><td>{p._2.name}</td>
        <td>{config.getString(c.id + "." + p._2.name + ".label")}</td>
        <td>{p._2.typeName.name}</td>
        <td>{p._2.defaultValue}</td>
      </tr>)
      val x = <table width="80%">{header}{f}</table>
      List(x.toString)
    }

    val s = doc.example.map { x =>
      new PrettyPrinter(1000, 1).format(toXml(docFile + ":" + c.id, x))
    }
    val x = s.map(t => <pre>{t}</pre>).map(x => new XmlPrettyPrinter(1000, 1).format(x))

    val x2 = if (x.isEmpty) {
      "TBD"
    } else {
      x.mkString("\nor\n")
    }

    val example = List("", "### Example configuration", x2)

    header ::: justifcation ::: description ::: parametersTitle ::: parameters ::: example
  }

  case class DocError(e: Exception)

  def generate(): Seq[String] = {
    val sdefStream = this.getClass.getClassLoader.getResourceAsStream(defnFile)
    val sdocStream = this.getClass.getClassLoader.getResourceAsStream(docFile)

    val scalastyleDefinition = ScalastyleDefinition.readFromXml(sdefStream)
    val config = ConfigFactory.load()

    val scalastyleDocumentation = (XML.load(sdocStream) \\ "check").map { ns =>
      (
        ns.attribute("id").get.text,
        Documentation(
          toText(ns \\ "justification"),
          toText(ns \\ "extra-description"),
          toList(ns \\ "example-configuration")
        )
      )
    }.toMap

    val fileHeader = List(
      "---",
      "layout: scalastyle",
      """title: "Scalastyle: Implemented Rules"""",
      "---",
      "",
      "There are " + scalastyleDefinition.checkers.size + " rules which are currently implemented:",
      ""
    )

    val checkers =
      scalastyleDefinition.checkers.sortWith((e1, e2) => (e1.className compareTo e2.className) < 0)

    val contentString = contents(checkers, config)

    val docs = checkers.flatMap { c =>
      val doc = scalastyleDocumentation.getOrElse(c.id, Documentation(None, None, Nil))
      checker(c, doc, config)
    }

    fileHeader ::: contentString ::: docs
  }

  private def generateToFile(file: String): Unit = {
    println("writing to " + file) // scalastyle:ignore println
    val contents = generate().mkString("\n")
    Files.write(Paths.get(file), contents.getBytes(StandardCharsets.UTF_8))
  }

  def main(args: Array[String]): Unit = {
    if (args.length == 0) {
      usage(BuildInfo.version)
    } else {
      try {
        generateToFile(args(0))
      } catch {
        case e: Exception => {
          e.printStackTrace(System.err)
          System.exit(1)
        }
      }
    }
  }
}
