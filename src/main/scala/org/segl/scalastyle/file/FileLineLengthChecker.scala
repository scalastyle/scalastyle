package org.segl.scalastyle.file

import java.lang.reflect.Constructor;
import org.segl.scalastyle.FileChecker
import org.segl.scalastyle.StyleError
import org.segl.scalastyle.Message
import org.segl.scalastyle.Lines
import org.segl.scalastyle._

// TODO add tab translation tab = 4 characters
class FileLineLengthChecker extends FileChecker {
  val DefaultMaxLineLength = 160
  val errorKey = "line.size.limit"

  def verify(lines: Lines): List[Position] = {
    val maxLineLength = getInt("maxLineLength", DefaultMaxLineLength)

    val errors = for (
      line <- lines.lines.zipWithIndex;
      if line._1.length() > maxLineLength
    ) yield {
      Position(Some(line._2 + 1))
    }

    errors.toList
  }
}
