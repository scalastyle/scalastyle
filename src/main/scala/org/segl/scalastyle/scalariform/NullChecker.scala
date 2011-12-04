package org.segl.scalastyle.scalariform

import java.lang.reflect.Constructor;
import scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.Tokens._
import org.segl.scalastyle.ScalariformChecker
import org.segl.scalastyle._

class NullChecker extends ScalariformChecker {
  val errorKey = "null"
    
  def verify(ast: CompilationUnit): List[Position] = {
    val it = for (
      List(left, right) <- ast.tokens.sliding(2);
      if (left.tokenType == NULL)
    ) yield {
      Position(position = Some(left.startIndex))
    }

    it.toList
  }
}