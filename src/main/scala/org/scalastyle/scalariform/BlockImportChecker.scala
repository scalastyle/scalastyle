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

import scalariform.lexer.Token
import scalariform.lexer.Tokens.ARROW
import scalariform.lexer.Tokens.USCORE
import scalariform.parser.{AstNode, BlockImportExpr, CompilationUnit, Expr, GeneralTokens, ImportClause, ImportSelectors}

class BlockImportChecker extends ScalariformChecker {

  val errorKey = "block.import"

  def verify(ast: CompilationUnit): List[ScalastyleError] =
    findBlockImports(ast)

  private def findBlockImports(in: AstNode): List[PositionError] = in match {

    // comma separated import
    case ImportClause(_, firstImport, otherImports) if otherImports.nonEmpty =>
      List(PositionError(firstImport.firstToken.offset))

    // rename or hide import
    case BlockImportExpr(prefix, ImportSelectors(_, Expr(
          List(_, GeneralTokens(List(Token(ARROW, "=>", _, _))), _)
        ), otherImports, _)) =>

      val blockImportFound = otherImports exists {
        case (_, Expr(List(GeneralTokens(List(Token(tokenType, _, _, _)))))) =>
          tokenType != USCORE
        case _ =>
          false
      }

      if (blockImportFound) List(PositionError(prefix.firstToken.offset)) else Nil

    // other block imports
    case b: BlockImportExpr => List(PositionError(b.firstToken.offset))

    // remaining nodes
    case a: AstNode => a.immediateChildren flatMap findBlockImports
  }

}
