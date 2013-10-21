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

import scalariform.lexer.Token
import scalariform.parser.{FunDefOrDcl, PatDefOrDcl, TmplDef, CasePattern}
import scala.Some
import org.scalastyle._
import org.scalastyle.Lines
import org.scalastyle.PositionError
import org.scalastyle.scalariform.VisitorHelper.visit
import _root_.scalariform.lexer.Tokens.COLON

/**
 * Check each tokens in type annotations
 */
abstract class TypeAnnotationTokenChecker extends CombinedChecker {
  val specialOperators = Set("*", "/", "%", "+", "-", ":", "=", "!", "<", ">", "&", "^", "|")

  protected def matcher(tokens: List[Token], lines: Lines): List[ScalastyleError]

  def verify(ast: CombinedAst): List[ScalastyleError] = {
    val tokenList = localvisit(ast.compilationUnit)
    tokenList.map(matcher(_, ast.lines)).flatten
  }

  def localvisit(ast: Any): List[List[Token]] = ast match {
    case t: TmplDef => checkClassDefinition(t) :: visit(t.immediateChildren, localvisit)
    case c: CasePattern => c.pattern.tokens :: visit(c.immediateChildren, localvisit)
    case v: PatDefOrDcl => checkPatternDefinition(v) :: visit(v.immediateChildren, localvisit)
    case f: FunDefOrDcl => checkFunctionDefinition(f) :: visit(f.funBodyOpt, localvisit)
    case a: Any => visit(a, localvisit)
  }

  private def checkClassDefinition(t: TmplDef): List[Token] = {
    val paramError = t.paramClausesOpt match {
      case Some(x) => x.tokens
      case None => Nil
    }
    val typeParamError = t.typeParamClauseOpt match {
      case Some(x) => x.tokens
      case None => Nil
    }
    typeParamError ::: paramError
  }

  private def checkPatternDefinition(v: PatDefOrDcl): List[Token] = v.typedOpt match {
    case Some((x, y)) =>
      if (specialOperators.exists(op => v.pattern.firstToken.text.startsWith(op))) {
        val shiftedPatternToken = v.pattern.tokens.map(t => t.copy(offset = t.offset + 1))
        shiftedPatternToken ::: x :: y.tokens
      } else {
        v.pattern.tokens ::: x :: y.tokens

      }
    case None => Nil
  }

  private def checkFunctionDefinition(f: FunDefOrDcl): List[Token] = f.returnTypeOpt match {
    case Some((x, y)) =>
      val namedToken = if (specialOperators.exists(op => f.nameToken.text.startsWith(op))) {
        f.nameToken.copy(offset = f.nameToken.offset + 1)
      } else {
        f.nameToken
      }
      f.typeParamClauseOpt match {
        case None => namedToken :: f.paramClauses.tokens ::: x :: y.tokens
        case Some(t) => namedToken :: t.tokens ::: f.paramClauses.tokens ::: x :: y.tokens
      }
    case None => f.paramClauses.tokens
  }
}

/**
 * Detects whether type annotations contain colons, and checks them by matcher.
 */
abstract class ColonChecker extends TypeAnnotationTokenChecker {

  def localMatcher(prev: Token, current: Token, next: Token, lines: Lines): Boolean

  override def matcher(tokens: List[Token], lines: Lines): List[PositionError] = {
    val it = for (
      List(prev, current, next) <- tokens.sliding(3);
      if (localMatcher(prev, current, next, lines))
    ) yield {
      PositionError(current.offset)
    }
    it.toList
  }

  def isSingleColonToken(current: Token, next: Token) =
    current.tokenType == COLON && next.tokenType != COLON

  def linesBetweenTokens(lines: Lines, srcToken: Token, dstToken: Token): Int =
    Math.abs(lines.toLineColumn(srcToken.offset).get.line - lines.toLineColumn(dstToken.offset).get.line)
}

/**
 * Checks no whitespace before colons
 */
class NoWhitespaceBeforeColonChecker extends ColonChecker {
  val errorKey = "no.whitespace.before.colon"
  val paramLineBreakAllowed = "lineBreakAllowed"

  def localMatcher(prev: Token, current: Token, next: Token, lines: Lines) = {
    val allowLineBreak = getBoolean(paramLineBreakAllowed, false)

    allowLineBreak match {
      case true =>
        isSingleColonToken(current, next) && !(charsBetweenTokens(prev, current) == 0 || linesBetweenTokens(lines, prev, current) == 1)
      case false =>
        isSingleColonToken(current, next) && !(charsBetweenTokens(prev, current) == 0)
    }
  }
}

/**
 * Checks only one whitespace after colons
 */
class WhitespaceAfterColonChecker extends ColonChecker {
  val errorKey = "whitespace.after.colon"
  val paramLineBreakAllowed = "lineBreakAllowed"

  def localMatcher(prev: Token, current: Token, next: Token, lines: Lines) = {
    val allowLineBreak = getBoolean(paramLineBreakAllowed, false)

    allowLineBreak match {
      case true =>
        isSingleColonToken(current, next) && !(charsBetweenTokens(current, next) == 1 || linesBetweenTokens(lines, current, next) == 1)
      case false =>
        isSingleColonToken(current, next) && !(charsBetweenTokens(current, next) == 1)
    }
  }
}
