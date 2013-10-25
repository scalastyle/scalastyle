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

import org.scalastyle.ColumnError
import org.scalastyle.FileChecker
import org.scalastyle.Lines
import org.scalastyle.ScalastyleError

class WhitespaceEndOfLineChecker extends FileChecker {
  val errorKey = "whitespace.end.of.line"

  private val whitespaces = Set(' ', '\t')
  private val endOfLines = Set('\n', '\r')

  private def endsWithWhitespace(s: String) = {
    val sb = s.reverse

    var endOfLinesIndex = 0;
    while (endOfLinesIndex < sb.length() && endOfLines(sb(endOfLinesIndex))) {
      endOfLinesIndex += 1
    }

    var whitespaceIndex = endOfLinesIndex;
    while (whitespaceIndex < sb.length() && whitespaces(sb(whitespaceIndex))) {
      whitespaceIndex += 1
    }

    (whitespaceIndex != endOfLinesIndex, s.length() - whitespaceIndex)
  }

  def verify(lines: Lines): List[ScalastyleError] = {
    val errors = for {
      line <- lines.lines.zipWithIndex;
      whitespace = endsWithWhitespace(line._1.text)
      if (whitespace._1)
    } yield {
      ColumnError(line._2 + 1, whitespace._2)
    }

    errors.toList
  }
}
