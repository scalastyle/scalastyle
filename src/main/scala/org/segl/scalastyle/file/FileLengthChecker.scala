package org.segl.scalastyle.file

import java.lang.reflect.Constructor;
import org.segl.scalastyle.FileChecker
import org.segl.scalastyle.StyleError
import org.segl.scalastyle.Message
import org.segl.scalastyle.Lines

class FileLengthChecker extends FileChecker {
  val DefaultMaxFileLength = 1000

  def verify(file: String, ast: Lines): List[Message] = {
    val maxLineLength = getInt("maxFileLength", DefaultMaxFileLength)

    if (ast.lines.size > maxLineLength) List(StyleError(file, "file.size.limit")) else List()
  }
}
