package org.segl.scalastyle.scalariform

import java.lang.reflect.Constructor;
import scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.Tokens._
import org.segl.scalastyle.ScalariformChecker
import org.segl.scalastyle._

class SpacesAfterPlusChecker extends ScalariformChecker {
  def verify(file: String, ast: CompilationUnit): List[Message] = {
    val it = for (
      List(left, right) <- ast.tokens.sliding(2);
      if (left.tokenType == PLUS && charsBetweenTokens(left, right) == 0)
    ) yield {
      StyleError(file, "spaces.after.plus", position = Some(left.startIndex))
    }

    it.toList
  }
}