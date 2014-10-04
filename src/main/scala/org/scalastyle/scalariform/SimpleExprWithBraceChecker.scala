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

import _root_.scalariform.parser._
import _root_.scalariform.lexer._
import _root_.scalariform.lexer.Tokens._
import org.scalastyle.{CombinedAst, CombinedChecker, ScalastyleError, PositionError, Lines}
import VisitorHelper.visit


/**
 * According to the Effective Scala "http://twitter.github.io/effectivescala/index.html#Formatting-Braces"
 * Braces are used to create compound expressions, using braces with simple statement is hard to read.
 * Effective Scala suggests avoid using braces with simple expression.
 *
 * this checker detects the simple expression where using braces like below,
 *
 * def foo() = {
 *  bar()
 * }
 *
 * the method above contains only one expression "bar()", so we can remove braces
 *
 * def foo() = bar()
 *
 * Configuration.
 * 1, To configure token to check, give parameter to checker in XML. like this,
 *
 * <parameters>
 *  <parameter name="targetTokens">def,for,while</parameter>
 * </parameter>
 *
 * To specify multiple tokens, write all expression name and separate them with ","
 * available tokens are def,val,var,if,else,for,while,do,case
 *
 * 2, To allow nested expression
 *
 * <parameter name="nestedAllowed">true</parameter>
 *
 * By default, the value is false
 */

class SimpleExprWithBraceChecker extends CombinedChecker {
  val errorKey = "simple.expression.with.brace"

  val ParamTargetTokens = "targetTokens"
  val ParamTargetTokensDefValue = ""

  val ParamNestedAllowed = "nestedAllowed"
  val ParamNestedAllowedDefValue = false

  val TargetTokensSeparator = ","

  case class SourceContext(lines: Lines, targetTokens: Set[TokenType], nestedAllowed: Boolean)

  override def verify(cAst: CombinedAst): List[ScalastyleError] = {
    val targetTokensString = getString(ParamTargetTokens, ParamTargetTokensDefValue)
    val targetTokens = (for (
      tokenString <- targetTokensString.split(TargetTokensSeparator)
    ) yield TokenType(tokenString.toUpperCase())).toSet
    val nestedAllowed = getBoolean(ParamNestedAllowed, ParamNestedAllowedDefValue)

    val sourceContext = SourceContext(cAst.lines, targetTokens, nestedAllowed)

    val it = for (Some(a) <- visit(cAst.compilationUnit, localvisit(_, sourceContext))) yield a

    it.toList
  }

  private def localvisit(ast: Any, sc: SourceContext): List[Option[ScalastyleError]] = ast match {
    case e: ExprElement => visitExprElement(e, sc)
    case d: DefOrDcl => visitDefOrDcl(d, sc)
    case c: CaseClause => visitCaseClause(c, sc)
    case a: Any => visit(a, localvisit(_, sc))
  }

  private def checkBlockExprHasSingleExpr(b: BlockExpr, nestedAllowed: Boolean): Option[ScalastyleError] =
    b.caseClausesOrStatSeq match {
      case Right(s: StatSeq) if s.otherStats.isEmpty && checkStatSeq(s, nestedAllowed) => Some(PositionError(b.lbrace.offset))
      case a: Any => None
    }

  private def checkStatSeq(ss: StatSeq, nestedAllowed: Boolean): Boolean = {
    val length = expandExpr(ss).length
    length <= 1 || (length > 1 && !nestedAllowed)
  }

  private def expandExpr(a: Any): List[CallExpr] = a match {
    case e: CallExpr => e :: visit(e, expandExpr)
    case a: Any => visit(a, expandExpr)
  }

  private def visitCaseClause(c: CaseClause, sc: SourceContext): List[Option[ScalastyleError]] = {
    val blockExprOrEmpty = c.statSeq.firstStatOpt match {
      case Some(x: Expr) => x.contents.take(1)
      case _ => List()
    }

    blockExprOrEmpty.flatMap {
      case x: BlockExpr => checkBlockExprHasSingleExpr(x, sc.nestedAllowed) :: visit(x, localvisit(_, sc))
      case _ => visit(c, localvisit(_, sc))
    }
  }

  private def visitDefOrDcl(d: DefOrDcl, sc: SourceContext): List[Option[ScalastyleError]] = d match {
    case f: FunDefOrDcl if sc.targetTokens.contains(DEF) => f.funBodyOpt match {
      case Some(e: ExprFunBody) => visitExprFunBody(e, sc)
      case _ => visit(f, localvisit(_, sc))
    }
    case p: PatDefOrDcl if sc.targetTokens.contains(p.valOrVarToken.tokenType) => visitPatDefOrDcl(p, sc)
    case a: Any => visit(a, localvisit(_, sc))
  }

  private def visitExprFunBody(e: ExprFunBody, sc: SourceContext): List[Option[ScalastyleError]] = e.body.contents match {
    case (x: BlockExpr) :: xs => checkBlockExprHasSingleExpr(x, sc.nestedAllowed) :: visit(e.body, localvisit(_, sc))
    case _ => visit(e.body, localvisit(_, sc))
  }

  private def visitPatDefOrDcl(p: PatDefOrDcl, sc: SourceContext): List[Option[ScalastyleError]] = p.equalsClauseOption match {
    case Some((_, e: Expr)) => e.contents match {
      case (x: BlockExpr) :: xs => checkBlockExprHasSingleExpr(x, sc.nestedAllowed) :: visit(e, localvisit(_, sc))
      case _ => visit(e, localvisit(_, sc))
    }
    case _ => visit(p, localvisit(_, sc))
  }

  private def visitExprElement(e: ExprElement, sc: SourceContext): List[Option[ScalastyleError]] = e match {
    case f: ForExpr => visitExpr(FOR, f.body, sc)
    case d: DoExpr => visitExpr(DO, d.body, sc)
    case w: WhileExpr => visitExpr(WHILE, w.body, sc)
    case i: IfExpr => visitIfExpr(i, sc)
    case _ => visit(e, localvisit(_, sc))
  }

  private def visitExpr(t: TokenType, e: Expr, sc: SourceContext): List[Option[ScalastyleError]] = e.contents match {
    case (x: BlockExpr) :: xs if sc.targetTokens.contains(t) => checkBlockExprHasSingleExpr(x, sc.nestedAllowed) :: visit(e, localvisit(_, sc))
    case _ => visit(e, localvisit(_, sc))
  }

  private def visitIfExpr(i: IfExpr, sc: SourceContext): List[Option[ScalastyleError]] = {
    val ifExprErrors = i.body.contents match {
      case (x: BlockExpr) :: xs if sc.targetTokens.contains(IF) =>
        checkBlockExprHasSingleExpr(x, sc.nestedAllowed) :: visit(i.body, localvisit(_, sc))
      case _ => visit(i.body, localvisit(_, sc))
    }

    val elseClauseErrors = i.elseClause match {
      case Some(x) => visitElseOrElseif(x, sc)
      case _ => List()
    }

    ifExprErrors ::: elseClauseErrors
  }

  private def visitElseOrElseif(e: ElseClause, sc: SourceContext): List[Option[ScalastyleError]] = e.elseBody.contents match {
    case (x: IfExpr) :: xs => visitIfExpr(x, sc) ::: visit(xs, localvisit(_, sc))
    case (x: BlockExpr) :: xs if sc.targetTokens.contains(ELSE) =>
      checkBlockExprHasSingleExpr(x, sc.nestedAllowed) :: visit(e.elseBody, localvisit(_, sc))
    case a: Any => visit(a, localvisit(_, sc))
  }
}
