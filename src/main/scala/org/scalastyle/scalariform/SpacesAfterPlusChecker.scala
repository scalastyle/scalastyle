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

import org.scalastyle.{PositionError, ScalariformChecker, ScalastyleError}
import org.scalastyle.scalariform.VisitorHelper.visit

import scalariform.lexer.Tokens.{LBRACKET, NEWLINE, PLUS}
import scalariform.parser.{CompilationUnit, InfixExpr}

class SpacesAfterPlusChecker extends ScalariformChecker {
  val errorKey = "spaces.after.plus"

  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val it = for {
      t <- localVisit(ast.immediateChildren(0))
      if isInvalidPlusInfix(t)
    } yield {
      PositionError(t.infixId.offset)
    }

    it.toList
  }

  private def isInvalidPlusInfix(infix: InfixExpr): Boolean = {
    if (infix.infixId.tokenType == PLUS) {
      val isLBracket = infix.left.lastOption.exists(_.tokens.lastOption.exists(_.tokenType == LBRACKET))
      val newLineExists = infix.newlineOption.exists(_.tokenType == NEWLINE)
      !isLBracket && !newLineExists && charsBetweenTokens(infix.infixId, infix.right.head.tokens.head) == 0
    } else {
      false
    }
  }

  private def localVisit(ast: Any): List[InfixExpr] = ast match {
    case expr: InfixExpr => List(expr)
    case other => visit(other, localVisit)
  }
}
