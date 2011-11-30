package org.segl.scalastyle.scalariform

import java.lang.reflect.Constructor;
import scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.Tokens._
import org.segl.scalastyle.ScalariformChecker
import org.segl.scalastyle._

class ClassNamesChecker extends ScalariformChecker {
  val DefaultRegex = "[A-Z][A-Za-z]*"

  def verify(file: String, ast: CompilationUnit): List[Message] = {
    val regex = getString("regex", DefaultRegex).r

    val it = for (
      List(left, right) <- ast.tokens.sliding(2);
      if (left.tokenType == CLASS && (regex findAllIn (right.getText)).size == 0)
    ) yield {
      StyleError(file, "class.name", position = Some(left.startIndex))
    }

    it.toList
  }
}

class ObjectNamesChecker extends ScalariformChecker {
  val DefaultRegex = "[A-Z][A-Za-z]*"

  def verify(file: String, ast: CompilationUnit): List[Message] = {
    val regex = getString("regex", DefaultRegex).r

    val it = for (
      List(left, right) <- ast.tokens.sliding(2);
      if (left.tokenType == OBJECT && (regex findAllIn (right.getText)).size == 0)
    ) yield {
      StyleError(file, "object.name", position = Some(left.startIndex))
    }

    it.toList
  }
}