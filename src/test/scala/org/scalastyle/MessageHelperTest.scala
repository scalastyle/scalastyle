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

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import com.typesafe.config.ConfigFactory
import com.typesafe.config.Config

// scalastyle:off multiple.string.literals

class MessageHelperTest extends AssertionsForJUnit {
  val classLoader = this.getClass().getClassLoader()
  val config = ConfigFactory.load()
  val definition = ScalastyleDefinition.readFromXml(classLoader.getResourceAsStream("scalastyle_definition.xml"))
  val messageHelper = new MessageHelper(config)

  // tests that for each class specified in scalastyle_definitions.xml
  // there exists all of the messages required,
  // i.e. label, message, description
  // depends upon MessageHelper returning the key if there isn't a key in scalastyle_messages
  @Test def testMessages(): Unit = {
    definition.checkers.foreach(c => {
      assertMessage(c.id, "message", messageHelper.message _)
      assertMessage(c.id, "label", messageHelper.label _)
      assertMessage(c.id, "description", messageHelper.description _)

      c.parameters.foreach(p => {
        val key = c.id + "." + p._1
        assertMessage(key, "label", messageHelper.description _)
        assertMessage(key, "description", messageHelper.description _)
      })
    })
  }

  private[this] def assertMessage(id: String, suffix: String, fn: (String, List[String]) => String) = {
    val key = id + "." + suffix
    assert(fn(id, List[String]()) != key, "checker " + id + " should have a (" + key + ")")
  }

  private[this] def assertMessage(id: String, suffix: String, fn: (String) => String) = {
    val key = id + "." + suffix
    assert(fn(id) != key, "checker " + id + " should have a (" + key + ")")
  }
}
