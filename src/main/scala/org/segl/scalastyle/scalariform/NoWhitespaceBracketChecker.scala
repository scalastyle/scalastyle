package org.segl.scalastyle.scalariform

import java.lang.reflect.Constructor;
import scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.Tokens._
import org.segl.scalastyle.ScalariformChecker
import org.segl.scalastyle._

class NoWhitespaceBeforeLeftBracketChecker extends ScalariformChecker {
  def verify(file: String, ast: CompilationUnit): List[Message] = {
    val it = for (
      List(left, right) <- ast.tokens.sliding(2);
      f = println("left=" + left + " right=" + right)
      if (right.tokenType == LBRACKET && charsBetweenTokens(left, right) > 0)
    ) yield {
      StyleError(file, "no.whitespace.before.left.bracket", position = Some(left.startIndex))
    }

    return it.toList
  }
}

class NoWhitespaceAfterLeftBracketChecker extends ScalariformChecker {
  def verify(file: String, ast: CompilationUnit): List[Message] = {
    val it = for (
      List(left, right) <- ast.tokens.sliding(2);
      f = println("left=" + left + " right=" + right)
      if (left.tokenType == LBRACKET && charsBetweenTokens(left, right) > 0)
    ) yield {
      StyleError(file, "no.whitespace.after.left.bracket", position = Some(left.startIndex))
    }

    return it.toList
  }
}