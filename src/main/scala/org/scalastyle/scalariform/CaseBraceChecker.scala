// Copyright (C) 2011-2018 the original author or authors.
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

import org.scalastyle.scalariform.VisitorHelper.getAll
import org.scalastyle.{CombinedAst, CombinedChecker, PositionError, ScalastyleError}
import scalariform.parser.{BlockExpr, CaseClause, Expr}

class CaseBraceChecker extends CombinedChecker {

  override protected val errorKey: String = "disallow.case.brace"

  override def verify(ast: CombinedAst): List[ScalastyleError] = {
    for {
      clause        <- getAll[CaseClause](ast.compilationUnit)
      blockPosition <- justBlockPosition(clause)
    } yield PositionError(blockPosition)
  }

  /**
   * Checks, if given case clause contains just block expression, without anything else.
   * @param clause case clause to check
   * @return position of block's left brace, if block found; None otherwise.
   */
  private def justBlockPosition(clause: CaseClause) = {
    import clause.statSeq._
    (firstStatOpt, otherStats.flatMap(_._2)) match {
      case (Some(Expr(List(block: BlockExpr))), Nil) => Some(block.lbrace.offset)
      case _                                         => None
    }
  }
}
