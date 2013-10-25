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

import scala.util.matching.Regex

import org.scalastyle.ColumnError
import org.scalastyle.FileChecker
import org.scalastyle.Lines
import org.scalastyle.ScalastyleError

class RegexChecker extends FileChecker {
  val errorKey = "regex"
  private val DefaultRegEx = ""

  def verify(lines: Lines): List[ScalastyleError] = {
    val file = (for {line <- lines.lines} yield line.text).mkString("\n")
    val regExpStr = getString("regex", DefaultRegEx)
    val regExp = new Regex(regExpStr)
    val allMatches = regExp.findAllIn(file)
    var errorList: List[ColumnError] = Nil

    while (allMatches.hasNext) {
      val location = allMatches.start
      allMatches.next()
      val matchedLine = findCorrespondingLine(location, file, lines)

      errorList = ColumnError(matchedLine + 1, findColumnPosition(location, lines, matchedLine), List(regExpStr)) :: errorList
    }

    errorList.reverse
  }

  private[this] def findCorrespondingLine(location: Int, data: String, lines: Lines): Int = {
    var line = 0
    var found = false

    while (!found){
      val currentLine = lines.lines(line)

      if (location >= currentLine.start && location < currentLine.end) {
        found = true
      } else {
        line += 1
      }
    }

    line
  }

  private[this] def findColumnPosition(location: Int, lines: Lines, matchedLine: Int): Int = {
    location - (lines.lines(matchedLine).start)
  }
}
