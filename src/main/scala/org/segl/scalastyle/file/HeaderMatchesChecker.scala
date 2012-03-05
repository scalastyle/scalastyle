package org.segl.scalastyle.file

import java.lang.reflect.Constructor;
import org.segl.scalastyle.FileChecker
import org.segl.scalastyle.StyleError
import org.segl.scalastyle.Message
import org.segl.scalastyle.Lines
import org.segl.scalastyle._

class HeaderMatchesChecker extends FileChecker {
  val errorKey = "header.matches"
  val DefaultHeader = ""

  def verify(ast: Lines): List[ScalastyleError] = {
    val header = Checker.parseLines(getString("header", DefaultHeader))

    val found = (0 to scala.math.min(ast.lines.size-1, header.lines.size-1)).find(i => !ast.lines(i).text.trim.equals(header.lines(i).text.trim))

    found match {
      case Some(x) => List(LineError(x + 1))
      case None => {
        if (ast.lines.size < header.lines.size) {
          List(LineError(ast.lines.size))
        } else {
          List()
        }
      }
    }
  }
}
