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

import java.lang.reflect.Constructor
import scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.Tokens._
import org.scalastyle.ScalariformChecker
import org.scalastyle._
import _root_.scalariform.parser._
import _root_.scalariform.lexer.Token

class RedundantIfChecker extends CombinedChecker {
  import VisitorHelper._
  val errorKey = "if.redundant"

  def verify(ast: CombinedAst): List[ScalastyleError] = {
    val it = for (
      t <- localvisit(ast.compilationUnit);
      if matches(t)
   ) yield {
      PositionError(t.firstToken.offset)
    }

    it.toList
  }

  private def matches(t: IfExpr): Boolean =
    isBoolean(t.body) && isBoolean(t.elseClause)

  private def isBoolean(t: Option[ElseClause]): Boolean = t match {
    case Some(ElseClause(None, tok, expr)) => tok.tokenType == ELSE && isBoolean(expr)
    case _ => false
  }
  private def isBoolean(t: Expr): Boolean = t match {
    case Expr(List(GeneralTokens(List(a)))) => isBoolean(a)
    case _ => false
  }
  private def isBoolean(t: Token): Boolean = Set(TRUE, FALSE).contains(t.tokenType)

  private def localvisit(ast: Any): List[IfExpr] = ast match {
    case t: IfExpr if matches(t) => List(t)
    case t: Any => visit(t, localvisit)
  }
}
