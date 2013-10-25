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
import org.scalastyle.LineColumn
import org.scalastyle.Lines
import org.scalastyle.PositionError
import org.scalastyle.ScalastyleError

import scalariform.parser.BlockExpr
import scalariform.parser.Expr
import scalariform.parser.IfExpr
import VisitorHelper.visit

class IfBraceChecker extends CombinedChecker {
  val DefaultSingleLineAllowed = true
  val DefaultDoubleLineAllowed = false
  val errorKey = "if.brace"

  def verify(ast: CombinedAst): List[ScalastyleError] = {
    val doubleLineAllowed = getBoolean("doubleLineAllowed", DefaultDoubleLineAllowed)
    val singleLineAllowed = doubleLineAllowed || getBoolean("singleLineAllowed", DefaultSingleLineAllowed)

    val it = for {
      t <- localvisit(ast.compilationUnit);
      f <- traverse(t, ast.lines, singleLineAllowed, doubleLineAllowed)
    } yield {
      PositionError(f.position.get)
    }

    it.toList
  }

  trait ExprTree[T] {
    def subs: List[T]
  }

  case class IfExprClazz(t: IfExpr, position: Option[Int], body: List[IfExprClazz], elseClause: List[IfExprClazz]) extends ExprTree[IfExprClazz] {
    def subs: List[IfExprClazz] = body ::: elseClause
  }

  private def traverse(t: IfExprClazz, lines: Lines, singleLineAllowed: Boolean, doubleLineAllowed: Boolean): List[IfExprClazz] = {
    val l = t.subs.map(traverse(_, lines, singleLineAllowed, doubleLineAllowed)).flatten
    if (matches(t, lines, singleLineAllowed, doubleLineAllowed)) t :: l else l
  }

  def matches(t: IfExprClazz, lines: Lines, singleLineAllowed: Boolean, doubleLineAllowed: Boolean): Boolean = {
    val ifLine = lines.toLineColumn(t.t.ifToken.offset)
    val ifBodyLine = firstLineOfGeneralTokens(t.t.body, lines)

    val (elseLine, elseBodyLine) = t.t.elseClause match {
      case Some(e) => (lines.toLineColumn(e.elseToken.offset), firstLineOfGeneralTokens(e.elseBody, lines))
      case None => (None, None)
    }

    if (ifBodyLine.isEmpty && (elseLine.isDefined && elseBodyLine.isEmpty)) return false;

    (ifLine, elseLine) match {
      case (Some(x), None) => if (singleLineAllowed) !sameLine(ifLine, ifBodyLine) else true
      case (Some(x), Some(y)) => {
        if (!sameLine(ifLine, ifBodyLine) || !sameLine(elseLine, elseBodyLine)) {
          true
        } else {
           if (sameLine(ifLine, elseLine)) !singleLineAllowed else !doubleLineAllowed
        }
      }
      case _ => false
    }
  }

  private[this] def sameLine(l1: Option[LineColumn], l2: Option[LineColumn]) = (l1, l2) match {
    case (Some(x), Some(y)) => x.line == y.line
    case _ => true
  }

  /** this returns Some(x) if we are NOT BlockExpr, i.e. there are no braces */
  private[this] def firstLineOfGeneralTokens(body: Expr, lines: Lines) = {
    if (body.contents.size > 0) {
      body.contents(0) match {
        case e: BlockExpr => None
        case e: IfExpr => None
        case e: Any => lines.toLineColumn(e.tokens(0).offset)
      }
    } else {
      None
    }
  }

  private def localvisit(ast: Any): List[IfExprClazz] = ast match {
    case t: IfExpr => List(IfExprClazz(t, Some(t.ifToken.offset), localvisit(t.body), localvisit(t.elseClause)))
    case t: Any => visit(t, localvisit)
  }
}
