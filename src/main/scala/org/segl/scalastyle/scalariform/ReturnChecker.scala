package org.segl.scalastyle.scalariform

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
  val errorKey = "return"
    
  def verify(ast: CompilationUnit): List[Position] = {
    val it = for (
      List(left, right) <- ast.tokens.sliding(2);
      if (left.tokenType == RETURN)
    ) yield {
      Position(position = Some(left.startIndex))
    }

    it.toList
  }

}