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
import org.junit.Ignore
import com.typesafe.config.ConfigFactory

// scalastyle:off magic.number multiple.string.literals

class OutputTest extends AssertionsForJUnit {
 @Test
 @Ignore("doesn't work under 2.9")
 def testXmlOutput(): Unit = {
   val fooSpec = new FileSpec { def name: String = "foo" }
   val barSpec = new FileSpec { def name: String = "bar" }

   val messages = List(
       StyleError(fooSpec, classOf[FileLengthChecker], "foobar", ErrorLevel, List[String](), Some(1), Some(2), Some("custom")),
       StyleError(fooSpec, classOf[FileLengthChecker], "foobar", ErrorLevel, List[String](), Some(3), Some(4), Some("custom 3")),
       StyleError(barSpec, classOf[FileLengthChecker], "barbar", WarningLevel, List[String](), None, None, None),
       StyleError(barSpec, classOf[FileLengthChecker], "bazbar", InfoLevel, List[String](), None, None, None),
       StyleException(barSpec, Some(classOf[FileLengthChecker]), "bazbaz", "stacktrace\nstacktrace", Some(5), Some(6)),
       StyleException(barSpec, None, "noClass", "stacktrace\nstacktrace", Some(7), Some(8))
   )

   new java.io.File("target").mkdir();
   new java.io.File("target/test").mkdir();

   XmlOutput.save(ConfigFactory.load(), "target/test/OutputTest.xml", "UTF-8", messages);

   val lineSep = System.getProperty("line.separator");
   val lines = scala.io.Source.fromFile(new java.io.File("target/test/OutputTest.xml")).getLines().mkString(lineSep)

   val expected = """<?xml version="1.0" encoding="UTF-8"?>
<checkstyle version="5.0">
 <file name="foo">
  <error column="2" line="1" source="org.scalastyle.file.FileLengthChecker" severity="error" message="custom"></error>
  <error column="4" line="3" source="org.scalastyle.file.FileLengthChecker" severity="error" message="custom 3"></error>
 </file>
 <file name="bar">
  <error source="org.scalastyle.file.FileLengthChecker" severity="warning" message="barbar.message"></error>
  <error source="org.scalastyle.file.FileLengthChecker" severity="info" message="bazbar.message"></error>
  <error column="6" line="5" source="org.scalastyle.file.FileLengthChecker" severity="error" message="bazbaz"></error>
  <error column="8" line="7" severity="error" message="noClass"></error>
 </file>
</checkstyle>"""

   assertEquals(expected, lines);
 }

 @Test def testXmlOutputCannotCreateFile(): Unit = {
   val fooSpec = new FileSpec { def name: String = "foo" }
   val barSpec = new FileSpec { def name: String = "bar" }

   val messages = List(
       StyleError(fooSpec, classOf[FileLengthChecker], "foobar", ErrorLevel, List[String](), Some(1), Some(2), Some("custom")),
       StyleError(fooSpec, classOf[FileLengthChecker], "foobar", ErrorLevel, List[String](), Some(3), Some(4), Some("custom 3")),
       StyleError(barSpec, classOf[FileLengthChecker], "barbar", WarningLevel, List[String](), None, None, None),
       StyleError(barSpec, classOf[FileLengthChecker], "bazbar", InfoLevel, List[String](), None, None, None),
       StyleException(barSpec, Some(classOf[FileLengthChecker]), "bazbaz", "stacktrace\nstacktrace", Some(5), Some(6)),
       StyleException(barSpec, None, "noClass", "stacktrace\nstacktrace", Some(7), Some(8))
   )

   new java.io.File("target").mkdir();

   try {
       XmlOutput.save(ConfigFactory.load(), "target/does.not.exist/OutputTest.xml", "UTF-8", messages);
   } catch {
     case e: java.io.FileNotFoundException => // OK
     case _: Throwable => fail("expected FileNotFoundException")
   }

 }
}
