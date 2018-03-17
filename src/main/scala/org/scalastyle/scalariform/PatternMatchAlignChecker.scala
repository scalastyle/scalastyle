// Copyright (C) 2011-2012 the original author or authors.
// See the LICENCE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package org.scalastyle.scalariform

import org.scalastyle.CombinedAst
import org.scalastyle.CombinedChecker
import org.scalastyle.Lines
import org.scalastyle.PositionError
import org.scalastyle.ScalastyleError
import org.scalastyle.scalariform.VisitorHelper.getAllRecursive

import _root_.scalariform.parser.BlockExpr
import _root_.scalariform.parser.CaseClauses

class PatternMatchAlignChecker extends CombinedChecker {
  val errorKey = "pattern.match.align"

  final def verify(ast: CombinedAst): List[ScalastyleError] = {
    val allBlockExprs = VisitorHelper.getAll[BlockExpr](ast.compilationUnit)
    val unaligned = allBlockExprs.filter(matches(_, ast.lines))
    unaligned.map { badBlock =>
      PositionError(badBlock.caseClausesOrStatSeq.left.get.caseClauses(1).casePattern.arrow.offset)
    }
  }

  def allAlign(clauses: CaseClauses, lines: Lines): Boolean = {
    val arrowPositions = clauses.caseClauses.map(clause => lines.toLineColumn(clause.casePattern.arrow.offset).map(_.column).getOrElse(-1))
    arrowPositions.forall(_ == arrowPositions.head)
  }

  private def matches(t: BlockExpr, lines: Lines) = {
    val isCaseClauses = t.caseClausesOrStatSeq.isLeft
    isCaseClauses && !allAlign(t.caseClausesOrStatSeq.left.get, lines)
  }

}
