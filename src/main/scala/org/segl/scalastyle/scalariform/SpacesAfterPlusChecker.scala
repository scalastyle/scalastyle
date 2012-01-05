package org.segl.scalastyle.scalariform

import java.lang.reflect.Constructor;
import scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.Tokens._
import org.segl.scalastyle.ScalariformChecker
import org.segl.scalastyle._

class SpacesAfterPlusChecker extends ScalariformChecker {
  val errorKey = "spaces.after.plus"

  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val it = for (
      List(left, middle, right) <- ast.tokens.sliding(3);
      if (middle.tokenType == PLUS && left.tokenType != LBRACKET && charsBetweenTokens(middle, right) == 0)
    ) yield {
      PositionError(middle.startIndex)
    }

    it.toList
  }
}