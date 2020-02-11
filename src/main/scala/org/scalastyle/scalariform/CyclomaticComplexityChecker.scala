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

import _root_.scalariform.lexer.Token
import _root_.scalariform.lexer.Tokens.CASE
import _root_.scalariform.lexer.Tokens.DO
import _root_.scalariform.lexer.Tokens.FOR
import _root_.scalariform.lexer.Tokens.IF
import _root_.scalariform.lexer.Tokens.MATCH
import _root_.scalariform.lexer.Tokens.VARID
import _root_.scalariform.lexer.Tokens.WHILE
import _root_.scalariform.parser.FunDefOrDcl
import org.scalastyle.CombinedAst
import org.scalastyle.CombinedChecker
import org.scalastyle.Lines
import org.scalastyle.PositionError
import org.scalastyle.ScalastyleError
import org.scalastyle.scalariform.VisitorHelper.Clazz
import org.scalastyle.scalariform.VisitorHelper.visit

class CyclomaticComplexityChecker extends CombinedChecker {
  val errorKey = "cyclomatic.complexity"
  val DefaultMaximum = 10
  val DefaultCountCases = true
  private lazy val maximum = getInt("maximum", DefaultMaximum)
  private lazy val countCases = getBoolean("countCases", DefaultCountCases)
  private val defaultTokens = Set(IF, WHILE, DO, FOR)

  case class FunDefOrDclClazz(t: FunDefOrDcl, position: Option[Int], subs: List[FunDefOrDclClazz])
      extends Clazz[FunDefOrDcl]()

  def verify(ast: CombinedAst): List[ScalastyleError] = {
    val it = for {
      t <- localvisit(ast.compilationUnit.immediateChildren.head)
      f <- traverse(t)
      value = matches(f, ast.lines, maximum)
      if value > maximum
    } yield {
      PositionError(t.position.get, List("" + value, "" + maximum))
    }

    it
  }

  private def traverse(t: FunDefOrDclClazz): List[FunDefOrDclClazz] = t :: t.subs.flatMap(traverse)

  private def isLogicalOrAnd(t: Token) = t.tokenType == VARID && (t.text == "&&" || t.text == "||")

  // compute the cyclomatic complexity without the additional 1
  private def cyclomaticComplexity(f: FunDefOrDclClazz): Int = {
    val tokens = defaultTokens + (if (countCases) CASE else MATCH)
    f.t.tokens.count(t => tokens.contains(t.tokenType) || isLogicalOrAnd(t))
  }

  private def matches(t: FunDefOrDclClazz, lines: Lines, maxLines: Int) = {
    val root = cyclomaticComplexity(t)
    val subs = t.subs.map(cyclomaticComplexity).sum
    root - subs + 1
  }

  private def localvisit(ast: Any): List[FunDefOrDclClazz] = ast match {
    case t: FunDefOrDcl => List(FunDefOrDclClazz(t, Some(t.nameToken.offset), visit(t, localvisit)))
    case t: Any         => visit(t, localvisit)
  }
}
