package org.segl.scalastyle.file

import java.lang.reflect.Constructor;
import org.segl.scalastyle.FileChecker
import org.segl.scalastyle.StyleError
import org.segl.scalastyle.Message
import org.segl.scalastyle.Lines
import org.segl.scalastyle._

class FileTabChecker extends FileChecker {
  val errorKey = "line.contains.tab"
    
  def verify(lines: Lines): List[Position] = {
    val errors = for (
      line <- lines.lines.zipWithIndex;
      if line._1.contains('\t')
    ) yield {
      Position(Some(line._2 + 1), Some(line._1.indexOf('\t')))
    }

    errors.toList
  }
}
