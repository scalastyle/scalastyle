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

package org.scalastyle.file

import scala.Array.canBuildFrom

import org.scalastyle.FileChecker
import org.scalastyle.LineError
import org.scalastyle.Lines
import org.scalastyle.ScalastyleError

class FileLineLengthChecker extends FileChecker {
  val DefaultMaxLineLength = 160
  val DefaultTabSize = 4
  val errorKey = "line.size.limit"

  private def spaces(column: Int, tabSize: Int): String = {
    val m = column % tabSize
    val length = if (m == 0) {
      tabSize
    } else {
      tabSize - m
    }

    String.format("%" + length + "s", " ")
  }

  private def replaceTabs(s: String, tabSize: Int): String = {
    val sb = new StringBuilder(s)
    val len = sb.length
    var i = 0;

    while (i < len) {
      if (sb.charAt(i) == '\t') {
        sb.replace(i, i + 1, spaces(i, tabSize))
      }
      i += 1
    }

    if (sb.endsWith("\r")) {
      sb.setLength(sb.length-1);
    }

    sb.toString
  }

  def verify(lines: Lines): List[ScalastyleError] = {
    val maxLineLength = getInt("maxLineLength", DefaultMaxLineLength)
    val tabSize = getInt("tabSize", DefaultTabSize)

    val errors = for (
      line <- lines.lines.zipWithIndex;
      if replaceTabs(line._1.text, tabSize).length() > maxLineLength
    ) yield {
      LineError(line._2 + 1, List("" + maxLineLength))
    }

    errors.toList
  }
}
