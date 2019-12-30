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

import java.lang.reflect.Modifier

import com.google.common.reflect.ClassPath
import com.typesafe.config.ConfigFactory
import org.junit.Test
import org.scalatestplus.junit.AssertionsForJUnit

import scala.collection.JavaConverters._
import scala.xml.Elem
import scala.xml.NodeSeq
import scala.xml.XML

// scalastyle:off multiple.string.literals

class ScalastyleDefinitionTest extends AssertionsForJUnit {

  private def isChecker(ci: ClassPath.ClassInfo) = {
    ci.getName.startsWith("org.scalastyle") && {
      val clazz = ci.load

      classOf[Checker[_]].isAssignableFrom(clazz) && !Modifier.isAbstract(clazz.getModifiers)
    }
  }

  @Test
  def checkAllCheckersInScalastyleDefinition(): Unit = {
    val classLoader = this.getClass.getClassLoader
    val config = ConfigFactory.load()

    val cp = ClassPath.from(this.getClass.getClassLoader)
    val subTypes = cp.getAllClasses.asList().asScala.filter(isChecker)
    val definition = ScalastyleDefinition.readFromXml(classLoader.getResourceAsStream("scalastyle_definition.xml"))
    val messageHelper = new MessageHelper(config)

    val checkers = definition.checkers

    val missing = subTypes.map(_.getName()).diff(checkers.map(c => c.className))

    assert(missing.isEmpty, "scalastyle_definition does not contain " + missing)

    val docXml = XML.load(this.getClass.getClassLoader.getResource("scalastyle_documentation.xml"))

    val scalastyleDocumentation: Map[String, Documentation] = (docXml \\ "check").map { ns =>
      val doc = Documentation(toText(ns \\ "justification"), toText(ns \\ "extra-description"), toList(ns \\ "example-configuration"))
      (ns.attribute("id").get.text, doc)
    }.toMap

    checkers.foreach { c =>
      val checker = Class.forName(c.className).newInstance().asInstanceOf[Checker[_]]

      val m = checker.getClass.getMethod("errorKey")
      val errorKey = m.invoke(checker)

      assert(errorKey == c.id, "errorKey and id do not match for " + c.className)

      assert(scalastyleDocumentation.get(c.id).isDefined, "errorKey and id does not have documentation")
      assert(scalastyleDocumentation(c.id).justification.isDefined, c.id + ": justification not defined")
      assert(scalastyleDocumentation(c.id).example.nonEmpty, c.id + ": example not defined")

      scalastyleDocumentation(c.id).example.foreach { e =>
        val ns: Elem = scala.xml.XML.loadString(e)

        assert(ns.attribute("class").map(_.text) == Some(c.className))
        assert(ns.attribute("enabled").map(_.text) == Some("true"))
        assert(ns.attribute("level").map(_.text) == Some("warning"))

        val params = ns \\ "parameters"
        if (params.nonEmpty) {
          val list = ns \\ "parameters" \\ "parameter"
          assert (list.size > 0, "if we have parameters, then we should have a list of parameter")
        }
      }
    }
  }

  private def toText(elem: NodeSeq) = if (elem.isEmpty) None else Some(elem.text.trim)
  private def toList(elem: NodeSeq) = elem.map(_.text.trim)

  case class Documentation(justification: Option[String], extraDescription: Option[String], example: Seq[String])
}
