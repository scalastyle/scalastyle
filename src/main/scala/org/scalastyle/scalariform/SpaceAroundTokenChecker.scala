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

import org.scalastyle.PositionError
import org.scalastyle.ScalariformChecker
import org.scalastyle.ScalastyleError

import _root_.scalariform.lexer.Token
import _root_.scalariform.lexer.TokenType
import _root_.scalariform.lexer.Tokens
import _root_.scalariform.parser.CompilationUnit

trait SpaceAroundTokenChecker extends ScalariformChecker {
  val DefaultTokens: String
  val disallowSpace: Boolean
  val beforeToken: Boolean

  private def checkSpaces(left: Token, middle: Token, right: Token) =
    if (beforeToken) {
      charsBetweenTokens(left, middle) != (if (disallowSpace) 0 else 1)
    } else {
      charsBetweenTokens(middle, right) != (if (disallowSpace) 0 else 1)
    }

  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val tokens: Seq[TokenType] = getString("tokens", DefaultTokens).split(",").map(x => TokenType(x.trim))
    (for {
      l @ List(left, middle, right) <- ast.tokens.sliding(3)
      if (l.forall(x => x.tokenType != Tokens.NEWLINE && x.tokenType != Tokens.NEWLINES)
        && tokens.contains(middle.tokenType)
        && !(middle.associatedWhitespaceAndComments.containsNewline && beforeToken)
        && (!right.associatedWhitespaceAndComments.containsNewline || beforeToken)
        && checkSpaces(left, middle, right))
    } yield {
      PositionError(middle.offset, List(middle.text))
    }).toList
  }

}

class EnsureSingleSpaceAfterTokenChecker extends SpaceAroundTokenChecker {
  val errorKey: String = "ensure.single.space.after.token"
  val DefaultTokens = "COLON, IF"
  val disallowSpace = false
  val beforeToken = false
}

class EnsureSingleSpaceBeforeTokenChecker extends SpaceAroundTokenChecker {
  val errorKey: String = "ensure.single.space.before.token"
  val DefaultTokens = ""
  val disallowSpace = false
  val beforeToken = true
}

class DisallowSpaceBeforeTokenChecker extends SpaceAroundTokenChecker {
  val errorKey: String = "disallow.space.before.token"
  val DefaultTokens = "COLON, COMMA, RPAREN"
  val disallowSpace = true
  val beforeToken = true
}

class DisallowSpaceAfterTokenChecker extends SpaceAroundTokenChecker {
  val errorKey: String = "disallow.space.after.token"
  val DefaultTokens = "LPAREN"
  val disallowSpace = true
  val beforeToken = false
}
