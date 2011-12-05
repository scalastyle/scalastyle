package org.segl.scalastyle.scalariform

import java.lang.reflect.Constructor;
import scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.Tokens._
import org.segl.scalastyle.ScalariformChecker
import org.segl.scalastyle._

class SpacesBeforePlusChecker extends ScalariformChecker {
  val errorKey = "spaces.before.plus"
    
  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val it = for (
      List(left, right) <- ast.tokens.sliding(2);
      if (right.tokenType == PLUS && charsBetweenTokens(left, right) == 0)
    ) yield {
      PositionError(right.startIndex)
    }

    it.toList
  }
}