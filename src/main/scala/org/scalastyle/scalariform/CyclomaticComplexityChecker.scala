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

import scalariform.lexer.Tokens.CASE
import scalariform.lexer.Tokens.DO
import scalariform.lexer.Tokens.FOR
import scalariform.lexer.Tokens.IF
import scalariform.lexer.Tokens.VARID
import scalariform.lexer.Tokens.WHILE
import scalariform.lexer.Token
import scalariform.parser.AstNode
import scalariform.parser.FunDefOrDcl

class CyclomaticComplexityChecker extends AbstractMethodChecker {
  val errorKey = "cyclomatic.complexity"
  val DefaultMaximum = 10
  private lazy val maximum = getInt("maximum", DefaultMaximum)
  private val tokens = Set(IF, CASE, WHILE, DO, FOR)

  override def params(t: BaseClazz[AstNode]): List[String] = List("" + cyclomaticComplexity(t), "" + maximum)

  def matches(t: BaseClazz[AstNode]): Boolean = {
    cyclomaticComplexity(t) > maximum
  }

  private def isLogicalOrAnd(t: Token) = t.tokenType == VARID && (t.text == "&&" || t.text == "||")

  def cyclomaticComplexity(t: BaseClazz[AstNode]): Int = {
    t match {
      case f: FunDefOrDclClazz => f.t.tokens.count(t => tokens.contains(t.tokenType) || isLogicalOrAnd(t)) + 1
      case _ => 0
    }
  }
}
