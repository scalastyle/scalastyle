package org.segl.scalastyle.file

import org.segl.scalastyle.{Message, StyleError, Lines, FileChecker}

/**
 * Checks that no return codes are present
 *
 * @author Galder Zamarreño
 */
class ReturnChecker extends FileChecker {

   def verify(file: String, lines: Lines): List[Message] = {
      val errors =
         for (line <- lines.lines.zipWithIndex; if line._1.contains("return"))
         yield {
            StyleError(file, "line.contains.return",
                       Some(line._2), Some(line._1.indexOf("return")))
         }
      errors.toList
   }

}