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

import org.scalastyle.file.FileLengthChecker

// scalastyle:off magic.number

class OutputTest extends AssertionsForJUnit {
  @Test def testXmlOutput(): Unit = {
    val fooSpec = new FileSpec { def name: String = "foo" }
    val barSpec = new FileSpec { def name: String = "bar" }

    val messages = List(
        StyleError(fooSpec, classOf[FileLengthChecker], "foobar", ErrorLevel, List[String](), Some(1), Some(2), Some("custom")),
        StyleError(fooSpec, classOf[FileLengthChecker], "foobar", ErrorLevel, List[String](), Some(3), Some(4), Some("custom 3")),
        StyleError(barSpec, classOf[FileLengthChecker], "barbar", WarningLevel, List[String](), None, None, None),
        StyleException(barSpec, Some(classOf[FileLengthChecker]), "bazbaz", "stacktrace\nstacktrace", Some(5), Some(6)),
        StyleException(barSpec, None, "noClass", "stacktrace\nstacktrace", Some(7), Some(8))
    )

    new java.io.File("target").mkdir();
    new java.io.File("target/test").mkdir();

    XmlOutput.save("target/test/OutputTest.xml", messages);

    val lineSep = System.getProperty("line.separator");
    val lines = scala.io.Source.fromFile(new java.io.File("target/test/OutputTest.xml")).getLines().mkString(lineSep)

    val expected = """<checkstyle version="5.0">
 <file name="bar">
  <error severity="warning" message="barbar.message" source="org.scalastyle.file.FileLengthChecker"></error>
  <error line="5" severity="error" message="bazbaz" source="org.scalastyle.file.FileLengthChecker" column="6"></error>
  <error line="7" severity="error" message="noClass" column="8"></error>
 </file>
 <file name="foo">
  <error line="1" severity="error" message="custom" source="org.scalastyle.file.FileLengthChecker" column="2"></error>
  <error line="3" severity="error" message="custom 3" source="org.scalastyle.file.FileLengthChecker" column="4"></error>
 </file>
</checkstyle>"""

    assertEquals(expected, lines);
  }
}
