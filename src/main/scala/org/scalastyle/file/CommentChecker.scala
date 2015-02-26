package org.scalastyle.file

import org.scalastyle.file.TodoCommentChecker._
import org.scalastyle.file.FixmeCommentChecker._
import org.scalastyle.{ColumnError, FileChecker, Lines, ScalastyleError}

/**
 * Abstract comment check for line comment style TODO or FIXME
 */
abstract class AbstractCommentChecker extends FileChecker {
  def DefaultRegex: String

  def verify(lines: Lines): List[ScalastyleError] = {
    val regexString = getString("regex", DefaultRegex)
    val regex = regexString.r
    val file = (for {line <- lines.lines} yield line.text).mkString("\n")
    val matches = regex.findAllIn(file)
    var errorList: List[ColumnError] = Nil

    while (matches.hasNext) {
      val location = matches.start
      matches.next()
      val matchedLine = findCorrespondingLine(location, file, lines)
      errorList = ColumnError(matchedLine + 1, findColumnPosition(location, lines, matchedLine), List(regexString)) :: errorList
    }
    errorList.reverse
  }

  private def findCorrespondingLine(location: Int, data: String, lines: Lines): Int = {
    var line = 0
    var found = false

    while (!found) {
      val currentLine = lines.lines(line)
      if (location >= currentLine.start && location < currentLine.end) found = true
      else line += 1
    }
    line
  }

  private def findColumnPosition(location: Int, lines: Lines, matchedLine: Int): Int = {
    location - lines.lines(matchedLine).start
  }
}

/**
 * Checks for line comment style TODOs
 */
class TodoCommentChecker extends AbstractCommentChecker {
  val errorKey = "todo.comment"
  override val DefaultRegex = TodoRegex
}

object TodoCommentChecker {
  val TodoRegex = """(?i)(//|/\*|/\*\*|\*)\s?todo(:?)\s+"""
}

/**
 * Checks for line comment style FIXMEs
 */
class FixmeCommentChecker extends AbstractCommentChecker {
  val errorKey = "fixme.comment"
  override val DefaultRegex = FixmeRegex
}

object FixmeCommentChecker {
  val FixmeRegex = """(?i)(//|/\*|/\*\*|\*)\s?fixme(:?)\s+"""
}
