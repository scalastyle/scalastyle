package org.segl.scalastyle.scalariform

import java.lang.reflect.Constructor;
import scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.Tokens._
import org.segl.scalastyle.ScalariformChecker
import org.segl.scalastyle._

class NoWhitespaceBeforeLeftBracketChecker extends ScalariformChecker {
  val errorKey = "no.whitespace.before.left.bracket"

  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val it = for (
      List(left, right) <- ast.tokens.sliding(2);
      if (right.tokenType == LBRACKET && charsBetweenTokens(left, right) > 0)
    ) yield {
      PositionError(left.startIndex)
    }

    it.toList
  }
}

class NoWhitespaceAfterLeftBracketChecker extends ScalariformChecker {
  val errorKey = "no.whitespace.after.left.bracket"

  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val it = for (
      List(left, right) <- ast.tokens.sliding(2);
      if (left.tokenType == LBRACKET && charsBetweenTokens(left, right) > 0)
    ) yield {
      PositionError(left.startIndex)
    }

    it.toList
  }
}