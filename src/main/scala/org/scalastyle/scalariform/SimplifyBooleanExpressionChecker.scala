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

import scalariform.parser.CompilationUnit
import scalariform.lexer.Tokens.VARID
import scalariform.lexer.Tokens.TRUE
import scalariform.lexer.Tokens.FALSE
import scalariform.lexer.Token
import org.scalastyle.ScalariformChecker
import org.scalastyle.ScalastyleError
import org.scalastyle.PositionError
import _root_.scalariform.parser.AstNode
import _root_.scalariform.parser.GeneralTokens
import _root_.scalariform.parser.InfixExpr
import _root_.scalariform.parser.PrefixExprElement
import VisitorHelper.{visit, Clazz}

class SimplifyBooleanExpressionChecker extends ScalariformChecker {
  val errorKey = "simplify.boolean.expression"

  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val it1 = for {
      List(left, right) <- ast.tokens.sliding(2);
      if (left.text == "!" && isBoolean(right))
    } yield {
      PositionError(left.offset)
    }

    val it2 = for {
      t <- localvisit(ast);
      if (matches(t))
    } yield {
      PositionError(t.position.get)
    }

    (it1.toList ::: it2.toList).sortWith((a, b) => a.position < b.position)
  }

  private def matches[T <: AstNode](t: Clazz[T]): Boolean = {
    t match {
      case t: InfixExprClazz => matchesInfixOp(t.id) && (boolean(t.left) || boolean(t.right))
      case _ => false
    }
  }

  private def matchesInfixOp(t: Token) = t.tokenType == VARID && Set("!=", "==", "&&", "||").contains(t.text)

  class BaseClazz[+T <: AstNode](val position: Option[Int]) extends Clazz[T]

  case class InfixExprClazz(_position: Option[Int], id: Token, left: List[Clazz[_]], right: List[Clazz[_]]) extends BaseClazz[InfixExpr](_position)
  case class PrefixExprElementClazz(_position: Option[Int], id: Token, expr: List[Clazz[_]]) extends BaseClazz[PrefixExprElement](_position)
  case class GeneralTokensClazz(_position: Option[Int], bool: Boolean) extends BaseClazz[GeneralTokens](_position)

  private def localvisit(ast: Any): List[BaseClazz[AstNode]] = ast match {
    case t: InfixExpr => List(InfixExprClazz(Some(t.firstToken.offset), t.infixId, localvisit(t.left), localvisit(t.right)))
    case t: GeneralTokens => List(GeneralTokensClazz(Some(t.firstToken.offset), isBoolean(t)))
    case t: Any => visit(t, localvisit)
  }

  private def boolean(expr: List[Clazz[_]]) = expr.size == 1 && expr(0).isInstanceOf[GeneralTokensClazz] && expr(0).asInstanceOf[GeneralTokensClazz].bool

  private def isBoolean(t: GeneralTokens): Boolean = t.tokens.size == 1 && isBoolean(t.tokens(0))
  private def isBoolean(t: Token): Boolean = Set(TRUE, FALSE).contains(t.tokenType)
}
