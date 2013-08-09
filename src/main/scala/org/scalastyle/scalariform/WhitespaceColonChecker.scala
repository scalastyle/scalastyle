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

import org.scalastyle._

import _root_.scalariform.lexer.Tokens.COLON
import org.scalastyle.scalariform.VisitorHelper._
import _root_.scalariform.parser.PatDefOrDcl
import _root_.scalariform.parser.FunDefOrDcl
import _root_.scalariform.parser.CasePattern
import _root_.scalariform.lexer.Token
import _root_.scalariform.parser.TmplDef
import _root_.scalariform.parser.CompilationUnit
import scala.Some
import org.scalastyle.PositionError

/**
 * Check each tokens in type annotations
 */
abstract class TypeAnnotationTokenChecker extends ScalariformChecker {
  val specialOperators = Set("*", "/", "%", "+", "-", ":", "=", "!", "<", ">", "&", "^", "|")

  protected def matcher(tokens: List[Token]): List[ScalastyleError]

  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    localvisit(ast)
  }

  def localvisit(ast: Any): List[ScalastyleError] = ast match {
    case t: TmplDef => checkClassDefinition(t, matcher) ::: visit(t.immediateChildren, localvisit)
    case c: CasePattern => matcher(c.pattern.tokens) ::: visit(c.immediateChildren, localvisit)
    case v: PatDefOrDcl => checkPatternDefinition(v, matcher) ::: visit(v.immediateChildren, localvisit)
    case f: FunDefOrDcl => checkFunctionDefinition(f, matcher) ::: visit(f.immediateChildren, localvisit)
    case a: Any => visit(a, localvisit)
  }

  private def checkClassDefinition(t: TmplDef, matcher: List[Token] => List[ScalastyleError]) = {
    val paramError = t.paramClausesOpt match {
      case Some(x) => matcher(x.tokens)
      case None => Nil
    }
    val typeParamError = t.typeParamClauseOpt match {
      case Some(x) => matcher(x.tokens)
      case None => Nil
    }
    typeParamError ::: paramError
  }

  private def checkPatternDefinition(v: PatDefOrDcl, matcher: List[Token] => List[ScalastyleError]) = v.typedOpt match {
    case Some((x, y)) =>
      if (specialOperators.exists(op => v.pattern.firstToken.text.startsWith(op))) {
        val shiftedPatternToken = v.pattern.tokens.map(t => t.copy(offset = t.offset + 1))
        matcher(shiftedPatternToken ::: x :: y.tokens)
      } else {
        matcher(v.pattern.tokens ::: x :: y.tokens)

      }
    case None => Nil
  }

  private def checkFunctionDefinition(f: FunDefOrDcl, matcher: List[Token] => List[ScalastyleError]) = f.returnTypeOpt match {
    case Some((x, y)) =>
      if (specialOperators.exists(op => f.nameToken.text.startsWith(op))) {
        val shiftedNameToken = f.nameToken.copy(offset = f.nameToken.offset + 1)
        matcher(shiftedNameToken :: f.paramClauses.tokens ::: x :: y.tokens)
      } else {
        matcher(f.nameToken :: f.paramClauses.tokens ::: x :: y.tokens)
      }
    case None => matcher(f.paramClauses.tokens)
  }
}

/**
 * Detects whether type annotations contain colons, and checks them by matcher.
 */
abstract class ColonChecker extends TypeAnnotationTokenChecker {

  def localMatcher(prev: Token, current: Token, next: Token): Boolean

  override def matcher(tokens: List[Token]): List[PositionError] = {
    val it = for (
      List(prev, current, next) <- tokens.sliding(3);
      if (localMatcher(prev, current, next))
    ) yield {
      PositionError(current.offset)
    }
    it.toList
  }

  def isSingleColonToken(current: Token, next: Token) =
    current.tokenType == COLON && next.tokenType != COLON
}

/**
 * Checks no whitespace before colons
 */
class NoWhitespaceBeforeColonChecker extends ColonChecker {
  val errorKey = "no.whitespace.before.colon"

  def localMatcher(prev: Token, current: Token, next: Token) =
    isSingleColonToken(current, next) && charsBetweenTokens(prev, current) != 0
}

/**
 * Checks only one whitespace after colons
 */
class WhitespaceAfterColonChecker extends ColonChecker {
  val errorKey = "whitespace.after.colon"

  def localMatcher(prev: Token, current: Token, next: Token) =
    isSingleColonToken(current, next) && charsBetweenTokens(current, next) != 1
}
