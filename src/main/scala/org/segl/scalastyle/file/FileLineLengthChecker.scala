package org.segl.scalastyle.file

import java.lang.reflect.Constructor;
import org.segl.scalastyle.FileChecker
import org.segl.scalastyle.StyleError
import org.segl.scalastyle.Message
import org.segl.scalastyle.Lines
import org.segl.scalastyle._

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
