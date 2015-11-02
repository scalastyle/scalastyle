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
import org.junit.Assert.assertFalse
import org.junit.Test
import org.scalastyle.file.FileLengthChecker
import org.junit.Ignore

// scalastyle:off magic.number multiple.string.literals line.size.limit

class MainTest extends AssertionsForJUnit {
  @Test def testParseArgs(): Unit = {
    testParseArgsError(Array("foo"))
    testParseArgs(Array("-c", "conf", "dir"), MainConfig(false, Some("conf"), None, List("dir"), false, false, false, None, None, None))
    testParseArgs(Array("--config", "conf", "dir"), MainConfig(false, Some("conf"), None, List("dir"), false, false, false, None, None, None))

    testParseArgs(Array("-v", "false", "-c", "conf", "dir"), MainConfig(false, Some("conf"), None, List("dir"), false, false, false, None, None, None))
    testParseArgs(Array("--verbose", "false", "-c", "conf", "dir"), MainConfig(false, Some("conf"), None, List("dir"), false, false, false, None, None, None))
    testParseArgs(Array("-v", "true", "-c", "conf", "dir"), MainConfig(false, Some("conf"), None, List("dir"), true, false, false, None, None, None))
    testParseArgs(Array("--verbose", "true", "-c", "conf", "dir"), MainConfig(false, Some("conf"), None, List("dir"), true, false, false, None, None, None))

    testParseArgs(Array("-q", "false", "-c", "conf", "dir"), MainConfig(false, Some("conf"), None, List("dir"), false, false, false, None, None, None))
    testParseArgs(Array("--quiet", "false", "-c", "conf", "dir"), MainConfig(false, Some("conf"), None, List("dir"), false, false, false, None, None, None))
    testParseArgs(Array("-q", "true", "-c", "conf", "dir"), MainConfig(false, Some("conf"), None, List("dir"), false, true, false, None, None, None))
    testParseArgs(Array("--quiet", "true", "-c", "conf", "dir"), MainConfig(false, Some("conf"), None, List("dir"), false, true, false, None, None, None))

    testParseArgs(Array("-w", "false", "-c", "conf", "dir"), MainConfig(false, Some("conf"), None, List("dir"), false, false, false, None, None, None))
    testParseArgs(Array("--warnings", "false", "-c", "conf", "dir"), MainConfig(false, Some("conf"), None, List("dir"), false, false, false, None, None, None))
    testParseArgs(Array("-w", "true", "-c", "conf", "dir"), MainConfig(false, Some("conf"), None, List("dir"), false, false, true, None, None, None))
    testParseArgs(Array("--warnings", "true", "-c", "conf", "dir"), MainConfig(false, Some("conf"), None, List("dir"), false, false, true, None, None, None))

    testParseArgs(Array("--xmlOutput", "xo", "-c", "conf", "dir"), MainConfig(false, Some("conf"), None, List("dir"), false, false, false, Some("xo"), None, None))
    testParseArgs(Array("--xmlEncoding", "xe", "-c", "conf", "dir"), MainConfig(false, Some("conf"), None, List("dir"), false, false, false, None, Some("xe"), None))
    testParseArgs(Array("--inputEncoding", "ie", "-c", "conf", "dir"), MainConfig(false, Some("conf"), None, List("dir"), false, false, false, None, None, Some("ie")))

    testParseArgs(Array("-e", "rules.jar", "-c", "conf", "dir"), MainConfig(false, Some("conf"), None, List("dir"), false, false, false, None, None, None, Some("rules.jar")))
    testParseArgs(Array("--externalJar", "rules.jar", "-c", "conf", "dir"), MainConfig(false, Some("conf"), None, List("dir"), false, false, false, None, None, None, Some("rules.jar")))

    testParseArgs(Array("-x", "foo/bar/*", "-c", "conf", "dir"), MainConfig(false, Some("conf"), None, List("dir"), excludedFiles = Seq("foo/bar/*")))
    testParseArgs(Array("--excludedFiles", "foo/bar/*", "-c", "conf", "dir"), MainConfig(false, Some("conf"), None, List("dir"), excludedFiles = Seq("foo/bar/*")))

    testParseArgsError(Array("-c", "conf"))
    testParseArgsError(Array("dir"))
  }

  @Test def testDoesntRunOnExcludedFilesAndSuppressionsFile(): Unit = {
    val config = Main.parseArgs(Array("-x", "foo/bar/*", "--suppressionsFile", "src/test/resources/config/suppressions.xml", "-c", "conf", "dir"))
    assertTrue(config.error)
  }

  @Test def testParseSuppressionFileInput_success(): Unit = {
    val config = Main.parseArgs(Array("--suppressionsFile", "src/test/resources/config/suppressions.xml", "-c", "conf", "dir"))
    assertFalse(config.error)
  }

  @Test def testParseSuppressionFileInput_failure(): Unit = {
    val config = Main.parseArgs(Array("--suppressionsFile", "src/test/resources/config/malformed_suppressions.xml", "-c", "conf", "dir"))
    assertTrue(config.error)
  }

  private def testParseArgs(args: Array[String], config: MainConfig) = {
    assert(Main.parseArgs(args) === config)
  }

  private def testParseArgsError(args: Array[String]) = {
    assert(Main.parseArgs(args).error === true)
  }

}
