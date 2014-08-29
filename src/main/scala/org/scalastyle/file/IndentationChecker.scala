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

import org.scalastyle.FileChecker
import org.scalastyle.LineError
import org.scalastyle.Line
import org.scalastyle.Lines
import org.scalastyle.ScalastyleError

object NormalizedLine {
  def normalize(lines: Lines, tabSize: Int): Array[NormalizedLine] =
    lines.lines.zipWithIndex map {
      case (line, index) => NormalizedLine(index + 1, line, tabSize)
    }
}

case class NormalizedLine(lineNumber: Int, line: Line, tabSize: Int) {
  lazy val normalizedText = replaceTabs(line.text, tabSize)
  lazy val length = normalizedText.length
  lazy val body = normalizedText dropWhile { _.isWhitespace }
  lazy val isBlank = { body.length == 0 }
  lazy val indentDepth = normalizedText prefixLength { _ == ' ' }

  def mkError(args: List[String] = Nil): LineError = LineError(lineNumber, args)

  // generates a string of spaces equal to the width of the tab
  private def spaces(column: Int, tabSize: Int): String = {
    val m = column % tabSize
    String.format("%" + (tabSize - m) + "s", " ")
  }

  // replaces tabs with spaces equal to the width of the tab
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
}

class IndentationChecker extends FileChecker {
  val DefaultTabSize = 2
  val errorKey = "indentation"

  private def multiLineComment(line: NormalizedLine) = line.body.startsWith("*")

  private def startsParamList(line: NormalizedLine) = line.body.matches(""".*class.*\([^\)]*""")

  // in multiline comments the last leading space is not part of the indent
  private def isTabAlligned(line: NormalizedLine): Boolean =
    (line.indentDepth % line.tabSize) == (if (multiLineComment(line)) 1 else 0)

  private def isSingleIndent(line: NormalizedLine, prior: NormalizedLine): Boolean =
    (line.indentDepth - prior.indentDepth) > line.tabSize

  private def verifyTabStop(lines: Seq[NormalizedLine]) =
    for { line <- lines if !isTabAlligned(line) } yield line.mkError()

  private def verifySingleIndent(lines: Seq[NormalizedLine]) =
    for { Seq(l1, l2) <- lines.sliding(2) if isSingleIndent(l2, l1) && !startsParamList(l1) } yield l2.mkError()

  def verify(lines: Lines): List[ScalastyleError] = {
    val tabSize = getInt("tabSize", DefaultTabSize)

    val normalizedLines = NormalizedLine.normalize(lines, tabSize) filterNot { _.isBlank }
    (verifyTabStop(normalizedLines) ++ verifySingleIndent(normalizedLines)).toList
  }
}
