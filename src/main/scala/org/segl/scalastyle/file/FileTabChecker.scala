package org.segl.scalastyle.file

import java.lang.reflect.Constructor;
import org.segl.scalastyle.FileChecker
import org.segl.scalastyle.StyleError
import org.segl.scalastyle.Message
import org.segl.scalastyle.Lines

class FileTabChecker extends FileChecker {
  def verify(file: String, lines: Lines): List[Message] = {
    val errors = for (
      line <- lines.lines.zipWithIndex;
      if line._1.contains('\t')
    ) yield {
      StyleError(file, "line.contains.tab", Some(line._2 + 1), Some(line._1.indexOf('\t')))
    }
    
    return errors.toList
  }
}
