package org.segl.scalastyle.scalariform

import java.lang.reflect.Constructor;
import scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.Tokens._
import org.segl.scalastyle.ScalariformChecker
import org.segl.scalastyle._
import org.segl.scalastyle.FileSpec

class ClassNamesChecker extends ScalariformChecker {
  val DefaultRegex = "[A-Z][A-Za-z]*"
  val errorKey = "class.name"

  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val regexString = getString("regex", DefaultRegex)
    val regex = regexString.r

    val it = for (
      List(left, right) <- ast.tokens.sliding(2);
      if (left.tokenType == CLASS && (regex findAllIn (right.getText)).size == 0)
    ) yield {
      PositionError(right.startIndex, List(regexString))
    }

    it.toList
  }
}

class ObjectNamesChecker extends ScalariformChecker {
  val DefaultRegex = "[A-Z][A-Za-z]*"
  val errorKey = "object.name"

  def verify(ast: CompilationUnit): List[PositionError] = {
    val regexString = getString("regex", DefaultRegex)
    val regex = regexString.r

    val it = for (
      List(left, right) <- ast.tokens.sliding(2);
      if (left.tokenType == OBJECT && (regex findAllIn (right.getText)).size == 0)
    ) yield {
      PositionError(right.startIndex, List(regexString))
    }

    it.toList
  }
}