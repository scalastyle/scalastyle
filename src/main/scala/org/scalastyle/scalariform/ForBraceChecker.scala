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

import org.scalastyle.{CombinedAst, CombinedChecker, Lines, PositionError, ScalastyleError}

import _root_.scalariform.lexer.Tokens
import _root_.scalariform.parser.ForExpr

class ForBraceChecker extends CombinedChecker {
  val errorKey = "for.brace"

  val defaultSingleLineAllowed = false
  lazy val singleLineAllowed: Boolean = getBoolean("singleLineAllowed", defaultSingleLineAllowed)

  final def verify(ast: CombinedAst): List[ScalastyleError] = {
    for {
      t <- VisitorHelper.getAll[ForExpr](ast.compilationUnit.immediateChildren.head)
      if !validSingleLine(t, ast.lines) && (
        Tokens.LPAREN == t.lParenOrBrace.tokenType ||
        Tokens.LPAREN == t.rParenOrBrace.tokenType
      )
    } yield PositionError(t.lParenOrBrace.offset)
  }

  private def validSingleLine(t: ForExpr, lines: Lines) = {
    singleLineAllowed && {
      val singleLine = for {
        start <- lines.toLineColumn(t.forToken.offset)
        end <- lines.toLineColumn(t.tokens.last.offset)
        if start.line == end.line
      } yield ()

      singleLine.isDefined
    }
  }
}
