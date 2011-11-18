package org.segl.scalastyle.file

import java.lang.reflect.Constructor;
import scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.Tokens._
import org.segl.scalastyle.ScalariformChecker
import org.segl.scalastyle._

/**
 * Checks that no return codes are present
 *
 * @author Galder Zamarreño
 */
class ReturnChecker extends ScalariformChecker {
  def verify(file: String, ast: CompilationUnit): List[Message] = {
    val it = for (
      List(left, right) <- ast.tokens.sliding(2);
      if (left.tokenType == RETURN)
    ) yield {
      StyleError(file, "return", position = Some(left.startIndex))
    }

    return it.toList
  }

}