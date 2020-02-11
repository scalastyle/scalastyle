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
import _root_.scalariform.lexer.TokenType
import _root_.scalariform.lexer.Tokens.INTEGER_LITERAL
import _root_.scalariform.lexer.Tokens.RETURN
import _root_.scalariform.lexer.Tokens.VARID
import _root_.scalariform.lexer.Tokens.WHILE
import _root_.scalariform.parser.CompilationUnit
import org.scalastyle.PositionError
import org.scalastyle.ScalariformChecker
import org.scalastyle.ScalastyleError

abstract class AbstractTokenChecker(val errorKey: String, tokenType: TokenType) extends ScalariformChecker {
  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val it = for {
      t <- ast.tokens
      if t.tokenType == tokenType && matches(t)
    } yield {
      PositionError(t.offset)
    }

    it
  }

  protected def matches(token: Token): Boolean = true
}

class UppercaseLChecker extends AbstractTokenChecker("uppercase.l", INTEGER_LITERAL) {
  override def matches(t: Token): Boolean = t.text.endsWith("l")
}

class WhileChecker extends AbstractTokenChecker("while", WHILE)
class ReturnChecker extends AbstractTokenChecker("return", RETURN)

class TokenChecker extends AbstractTokenChecker("token", VARID) {
  private val DefaultRegex = "^$"
  lazy val regex = getString("regex", DefaultRegex).r

  override protected def matches(t: Token) = regex.findFirstIn(t.text).isDefined
}
