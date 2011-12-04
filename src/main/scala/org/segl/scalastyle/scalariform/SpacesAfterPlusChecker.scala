package org.segl.scalastyle.scalariform

import java.lang.reflect.Constructor;
import scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.Tokens._
import org.segl.scalastyle.ScalariformChecker
import org.segl.scalastyle._

class SpacesAfterPlusChecker extends ScalariformChecker {
  val errorKey = "spaces.after.plus"
    
  def verify(ast: CompilationUnit): List[Position] = {
    val it = for (
      List(left, right) <- ast.tokens.sliding(2);
      if (left.tokenType == PLUS && charsBetweenTokens(left, right) == 0)
    ) yield {
      Position(position = Some(left.startIndex))
    }

    it.toList
  }
}