package org.segl.scalastyle.scalariform

import java.lang.reflect.Constructor;
import scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.Tokens._
import org.segl.scalastyle.ScalariformChecker
import org.segl.scalastyle._

class MagicNumberChecker extends ScalariformChecker {
  val DefaultIgnore = "-1,0,1,2"

  def verify(file: String, ast: CompilationUnit): List[Message] = {
    val ignores = getString("ignore", DefaultIgnore).split(",").toSet
    
    val it = for (
      t <- ast.tokens;
      if (t.tokenType == INTEGER_LITERAL && !ignores.contains(t.getText))
    ) yield {
      StyleError(file, "magic.number", position = Some(t.startIndex))
    }

    return it.toList
  }
}