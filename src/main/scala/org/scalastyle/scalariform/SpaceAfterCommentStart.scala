package org.scalastyle.scalariform

import _root_.scalariform.parser.CompilationUnit
import _root_.scalariform.lexer._
import org.scalastyle.{PositionError, ScalastyleError, ScalariformChecker}
import java.util.regex.Pattern

class SpaceAfterCommentStart extends ScalariformChecker {

  override protected val errorKey: String = "space.after.comment.start"
  val multiLinePattern = Pattern.compile("""/\*\S+.*""", Pattern.DOTALL)
  val scalaDocPattern = Pattern.compile("""/\*\*\S+.*""", Pattern.DOTALL)
  override def verify(ast: CompilationUnit): List[ScalastyleError] = {
    ast.tokens
      .filter(x => x.associatedWhitespaceAndComments != null && !x.associatedWhitespaceAndComments.comments.isEmpty)
      .map {
      _.associatedWhitespaceAndComments.comments.map {
        case x: SingleLineComment => if (x.token.text.trim.matches("""//\S+.*""")) x.token.offset else -1
        case x: MultiLineComment => if (multiLinePattern.matcher(x.token.text.trim).matches()) x.token.offset else -1
        case x: ScalaDocComment => if (scalaDocPattern.matcher(x.token.text.trim).matches()) x.token.offset else -1
        case y => -1
      }
    }.flatten.filter(_ != -1) map (PositionError(_))
  }
}
