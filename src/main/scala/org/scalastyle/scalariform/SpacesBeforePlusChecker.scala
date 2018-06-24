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

import org.scalastyle.ScalametaChecker
import org.scalastyle.ScalastyleError

import scala.meta.Tree
import scala.meta.tokens.Token

class SpacesBeforePlusChecker extends ScalametaChecker {
  val errorKey = "spaces.before.plus"

  def verify(ast: Tree): Seq[ScalastyleError] = {
    val it = for {
      Array(left, right) <- ast.tokens.tokens.sliding(2)
      if isIdent(right, "+") && !isLeftBracket(left) && !isLF(left) && !isSpace(left)
    } yield {
      toError(right)
    }

    it.toList
  }

  private def isIdent(ident: Token, text: String): Boolean = ident match {
    case i: Token.Ident => i.text == text
    case _ => false
  }

  private def isLF(token: Token): Boolean = token.isInstanceOf[Token.LF]
  private def isSpace(token: Token): Boolean = token.isInstanceOf[Token.Space]
  private def isLeftBracket(token: Token): Boolean = token.isInstanceOf[Token.LeftBracket]
}
