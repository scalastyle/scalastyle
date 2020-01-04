package org.scalastyle.scalariform

import _root_.scalariform.lexer.Tokens
import _root_.scalariform.parser.ForExpr
import org.scalastyle.{CombinedAst, CombinedChecker, PositionError, ScalastyleError}

class ForLoopChecker extends CombinedChecker {
  val errorKey = "for.loop"

  final def verify(ast: CombinedAst): List[ScalastyleError] = {
    for {
      t <- VisitorHelper.getAll[ForExpr](ast.compilationUnit.immediateChildren.head)
      if isLoop(t) && !(
        Tokens.LPAREN == t.lParenOrBrace.tokenType && Tokens.RPAREN == t.rParenOrBrace.tokenType
      )
    } yield PositionError(t.lParenOrBrace.offset)
  }

  private def isLoop(t: ForExpr) = t.yieldOption.isEmpty
}
