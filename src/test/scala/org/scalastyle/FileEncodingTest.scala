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
import java.io.BufferedWriter;
import java.io.OutputStreamWriter;
import java.io.FileOutputStream;

class FileEncodingTest extends AssertionsForJUnit {
  val TestString = "foobaré¨à$éèè¨è¨"

  @Test def testFileEncodings() {
    assertEquals(TestString, Checker.readFile(createFile("UTF16")))
    assertEquals(TestString, Checker.readFile(createFile("UTF8")))
    Checker.readFile(createFile("ISO-8859-1")) // can't tell the difference between UTF-8 and ISO-8859-1
    Checker.readFile(createFile("windows-1252")) // can't tell the difference between UTF-8 and windows-1252
    assertEquals(TestString, Checker.readFile(createFile("UTF-16BE")))
    Checker.readFile(createFile("UTF-16LE")) // can't tell the difference between UTF-16 and UTF-16LE
    Checker.readFile(createFile("GBK")) // gets read by ISO-8859-1
  }

  private def createFile(encoding: String) = {
    val filename = "target/test/fileEncodingTest." + encoding + ".txt";

    new java.io.File("target").mkdir();
    new java.io.File("target/test").mkdir();

    val out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(filename), encoding));
    out.write(TestString)
    out.close()

    filename
  }

}
