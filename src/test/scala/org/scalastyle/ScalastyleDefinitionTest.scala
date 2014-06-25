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
import scala.collection.convert.WrapAsScala.asScalaSet
import org.junit.Test
import org.reflections.Reflections
import org.scalatest.junit.AssertionsForJUnit
import com.typesafe.config.ConfigFactory
import java.lang.reflect.Method

// scalastyle:off multiple.string.literals

class ScalastyleDefinitionTest extends AssertionsForJUnit {

  @Test
  def checkAllCheckersInScalastyleDefinition(): Unit = {
    val classLoader = this.getClass().getClassLoader()
    val config = ConfigFactory.load()

    val reflections = new Reflections("org.scalastyle")
    val subTypes = asScalaSet(reflections.getSubTypesOf(classOf[Checker[_]])).filter(st => !Modifier.isAbstract(st.getModifiers())).toList
    val definition = ScalastyleDefinition.readFromXml(classLoader.getResourceAsStream("scalastyle_definition.xml"))
    val messageHelper = new MessageHelper(config)

    val checkers = definition.checkers

    val missing = subTypes.map(_.getName()).diff(checkers.map(c => c.className))

    assert(missing.isEmpty, "scalastyle_definition does not contain " + missing)

    checkers.foreach { c =>
      val checker = Class.forName(c.className).newInstance().asInstanceOf[Checker[_]]

      val m = checker.getClass().getMethod("errorKey")
      val errorKey = m.invoke(checker)

      assert(errorKey == c.id, "errorKey and id do not match for " + c.className)
    }


  }

}
