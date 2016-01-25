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

import org.junit.Test
import org.scalastyle.file.{CheckerTestHelper, FileLengthChecker}
import org.junit.Assert.assertEquals

class CheckerTest {
  val scalastyleChecker = new ScalastyleChecker[FileSpec](Some(this.getClass.getClassLoader))

  @Test def testRunOnEmptyRuleList(): Unit = {
    val source = """
  object Foo {
  }
"""
    val config = ScalastyleConfiguration("a name", false, List.empty)
    val sourceSpec: FileSpec = new SourceSpec("somename.scala", source)
    val thing = FileNameAndRules(sourceSpec, config)
    val filesAndRules = Seq(thing)
    val result = scalastyleChecker.completeAllFileChecks(filesAndRules)

    assertEquals(4, result.size)
    assertEquals(new StartWork[SourceSpec](), result(0))
    assertEquals(sourceSpec, result(1).asInstanceOf[StartFile[SourceSpec]].fileSpec)
    assertEquals(sourceSpec, result(2).asInstanceOf[EndFile[SourceSpec]].fileSpec)
    assertEquals(new EndWork[SourceSpec](), result(3))
  }
}
