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
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test
import org.junit.Ignore
import org.scalastyle.file.HeaderMatchesChecker

class ScalastyleConfigurationTest extends AssertionsForJUnit {
  val filename = "src/test/resources/config/scalastyle_config.xml"
  val width = 1000
  val step = 1

  // just check we can read it
  @Test def readXml(): Unit = {
    val config = ScalastyleConfiguration.readFromXml(filename)
  }

  @Ignore
  @Test def writeXml(): Unit = {
    val config = ScalastyleConfiguration.readFromXml(filename)
    val contents = scala.io.Source.fromFile(filename).mkString

    assertEquals(clean(contents), clean(ScalastyleConfiguration.toXmlString(config, width, step)))
  }

  def clean(s: String): String = s.replace("\015", "")
}
