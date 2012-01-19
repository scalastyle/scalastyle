package org.segl.scalastyle.file

import java.lang.reflect.Constructor;
import org.segl.scalastyle.FileChecker
import org.segl.scalastyle.StyleError
import org.segl.scalastyle.Message
import org.segl.scalastyle.Lines
import org.segl.scalastyle._

class FileLengthChecker extends FileChecker {
  val errorKey = "file.size.limit"
  val DefaultMaxFileLength = 1000

  def verify(ast: Lines): List[ScalastyleError] = {
    val maxFileLength = getInt("maxFileLength", DefaultMaxFileLength)

    if (ast.lines.size > maxFileLength) List(FileError(List("" + maxFileLength))) else List()
  }
}
