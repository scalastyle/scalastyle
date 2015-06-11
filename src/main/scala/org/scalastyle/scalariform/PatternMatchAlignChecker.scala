package org.scalastyle.scalariform


import org.scalastyle._
import org.scalastyle.scalariform.VisitorHelper._

import _root_.scalariform.lexer.Token
import _root_.scalariform.lexer.Tokens._
import _root_.scalariform.parser._



class PatternMatchAlignChecker extends CombinedChecker {
  val errorKey = "pattern.match.align"

  final def verify(ast: CombinedAst): List[ScalastyleError] = {
    val allBlockExprs = getAllRecursive[BlockExpr](ast.compilationUnit)
    val unaligned = allBlockExprs.filter(matches(_, ast.lines))
    unaligned.map { badBlock =>
      PositionError(badBlock.caseClausesOrStatSeq.left.get.caseClauses(1).casePattern.arrow.offset)
    }
  }

  def allAlign(clauses: CaseClauses, lines:Lines): Boolean = {
    val arrowPositions = clauses.caseClauses.map(clause => lines.toLineColumn(clause.casePattern.arrow.offset).map(_.column).getOrElse(-1))
    arrowPositions.forall(_ == arrowPositions(0))
  }

  private def matches(t: BlockExpr, lines: Lines) = {
    val isCaseClauses = t.caseClausesOrStatSeq.isLeft
    isCaseClauses && !allAlign(t.caseClausesOrStatSeq.left.get, lines)
  }

}
