package org.segl.scalastyle.file

import java.lang.reflect.Constructor;
import org.segl.scalastyle.FileChecker
import org.segl.scalastyle.StyleError
import org.segl.scalastyle.Message
import org.segl.scalastyle.Lines
import org.segl.scalastyle._

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
    val errors = for (
      line <- lines.lines.zipWithIndex;
      whitespace = endsWithWhitespace(line._1.text)
      if (whitespace._1)
    ) yield {
      ColumnError(line._2 + 1, whitespace._2)
    }

    errors.toList
  }
}
