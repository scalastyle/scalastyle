package org.segl.scalastyle.file

import java.lang.reflect.Constructor;
import org.segl.scalastyle.FileChecker
import org.segl.scalastyle.StyleError
import org.segl.scalastyle.Message
import org.segl.scalastyle.Lines

// TODO add tab translation tab = 4 characters
class FileLineLengthChecker extends FileChecker {
  val DefaultMaxLineLength = 160
  
  def verify(file: String, lines: Lines): List[Message] = {
    val maxLineLength = getInt("maxLineLength", DefaultMaxLineLength)
    
    val errors = for (
      line <- lines.lines.zipWithIndex;
      if line._1.length() > maxLineLength
    ) yield {
      StyleError(file, "line.size.limit", Some(line._2 + 1))
    }
    
    return errors.toList
  }
}
