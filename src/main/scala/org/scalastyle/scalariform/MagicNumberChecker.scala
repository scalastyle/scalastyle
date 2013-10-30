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

import scala.Option.option2Iterable

import org.scalastyle.scalariform.VisitorHelper.Clazz
import org.scalastyle.PositionError
import org.scalastyle.ScalariformChecker
import org.scalastyle.ScalastyleError

import scalariform.lexer.Tokens.INTEGER_LITERAL
import scalariform.lexer.Tokens.VAL
import scalariform.lexer.Token
import scalariform.parser.CompilationUnit
import scalariform.parser.Expr
import scalariform.parser.ExprElement
import scalariform.parser.GeneralTokens
import scalariform.parser.PatDefOrDcl
import scalariform.parser.PrefixExprElement

class MagicNumberChecker extends ScalariformChecker {
  val DefaultIgnore = "-1,0,1,2"
  val errorKey = "magic.number"

  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val ignores = getString("ignore", DefaultIgnore).split(",").toSet

    val intList = for {
      t <- localvisit(ast.immediateChildren(0));
      f <- traverse(t);
      if (matches(f, ignores))
    } yield {
      f
    }

    val valList = (for {
      t <- localvisitVal(ast.immediateChildren(0));
      f <- traverseVal(t);
      g <- toOption(f)
    } yield {
      g
    }).map( d => d match {
      case Expr(List(t: Expr)) => t
      case _ => d
    })

    intList.filter(t => !valList.contains(t.t)).map(t => PositionError(t.position)).toList
  }

  case class ExprVisit(t: Expr, position: Int, contents: List[ExprVisit]) extends Clazz[Expr]()

  private def traverse(t: ExprVisit): List[ExprVisit] = t :: t.contents.map(traverse(_)).flatten

  private def matches(t: ExprVisit, ignores: Set[String]) = {
    toIntegerLiteralExprElement(t.t.contents) match {
      case Some(x) => !ignores.contains(x)
      case None => false
    }
  }

  private def toIntegerLiteralExprElement(list: List[ExprElement]): Option[String] = {
    list match {
      case List(Expr(List(PrefixExprElement(t), GeneralTokens(gtList)))) => toIntegerLiteral(t, toIntegerLiteralToken(gtList))
      case List(PrefixExprElement(t), GeneralTokens(gtList)) => toIntegerLiteral(t, toIntegerLiteralToken(gtList))
      case List(GeneralTokens(gtList)) => toIntegerLiteralToken(gtList)
      case _ => None
    }
  }

  private def toIntegerLiteral(prefixExpr: Token, intLiteral: Option[String]): Option[String] = {
    (prefixExpr.text, intLiteral) match {
      case ("+", Some(i)) => Some(i)
      case ("-", Some(i)) => Some("-" + i)
      case _ => None
    }
  }

  private def toIntegerLiteralToken(list: List[Token]): Option[String] = {
    list match {
      case List(Token(tokenType, text, start, end)) if (tokenType == INTEGER_LITERAL) => Some(text.replaceAll("[Ll]", ""))
      case _ => None
    }
  }

  private def localvisit(ast: Any): List[ExprVisit] = ast match {
    case Expr(List(t: Expr)) => List(ExprVisit(t, t.firstToken.offset, localvisit(t.contents)))
    case t: Expr => List(ExprVisit(t, t.firstToken.offset, localvisit(t.contents)))
    case t: Any => VisitorHelper.visit(t, localvisit)
  }

  case class PatDefOrDclVisit(t: PatDefOrDcl, valOrVarToken: Token, pattern: List[PatDefOrDclVisit], otherPatterns: List[PatDefOrDclVisit],
                              equalsClauseOption: List[PatDefOrDclVisit])

  private def localvisitVal(ast: Any): List[PatDefOrDclVisit] = ast match {
    case t: PatDefOrDcl => List(PatDefOrDclVisit(t, t.valOrVarToken, localvisitVal(t.pattern),
                                    localvisitVal(t.otherPatterns), localvisitVal(t.equalsClauseOption)))
    case t: Any => VisitorHelper.visit(t, localvisitVal)
  }

  private def traverseVal(t: PatDefOrDclVisit): List[PatDefOrDclVisit] = t :: t.equalsClauseOption.map(traverseVal(_)).flatten

  private def toOption(t: PatDefOrDclVisit): Option[Expr] = {
    t.t.equalsClauseOption match {
      case Some((equals: Token, expr: Expr)) if (t.t.valOrVarToken.tokenType == VAL && toIntegerLiteralExprElement(expr.contents).isDefined) => Some(expr)
      case _ => None
    }
  }
}
