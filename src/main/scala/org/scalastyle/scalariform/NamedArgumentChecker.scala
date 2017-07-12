package org.scalastyle.scalariform

import org.scalastyle._

import _root_.scalariform.lexer.{ Token, Tokens }
import _root_.scalariform.parser._
import scala.util.matching.Regex

/**
 * Checks method calls to ensure that passed literals are named.
 */
class NamedArgumentChecker extends ScalariformChecker {
  protected val errorKey: String = "named.argument"
  val DefaultCheckString = false
  val DefaultIgnoreMethod = "^set.+$"

  override def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val checkString = getBoolean("checkString", DefaultCheckString)
    val ignoreMethod = getString("ignoreMethod", DefaultIgnoreMethod)

    localVisit(checkString, ignoreMethod.r)(ast.immediateChildren.head)
  }

  private def localVisit(checkString: Boolean, ignoreMethod: Regex)(ast: Any): List[ScalastyleError] = ast match {
    case CallExpr(_, Token(_, ignoreMethod(), _, _), _, _, _) =>
      Nil

    case Argument(Expr(List(GeneralTokens(List(Token(tokenType, _, offset, _)))))) =>
      val namelessLiteral = tokenType match {
        case Tokens.NULL |
             Tokens.FALSE | Tokens.TRUE |
             Tokens.INTEGER_LITERAL |
             Tokens.CHARACTER_LITERAL |
             Tokens.FLOATING_POINT_LITERAL =>
          true
        case Tokens.STRING_LITERAL =>
          checkString
        case _ =>
          false
      }
      if (namelessLiteral) {
        List(PositionError(offset))
      } else {
        Nil
      }

    case Argument(Expr(List(StringInterpolation(Token(_, _, offset, _), _, _)))) if checkString =>
      List(PositionError(offset))

    case t =>
      VisitorHelper.visit(t, localVisit(checkString, ignoreMethod))
  }
}
