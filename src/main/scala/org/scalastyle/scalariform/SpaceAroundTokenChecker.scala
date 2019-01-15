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

import org.scalastyle.CombinedMeta
import org.scalastyle.CombinedMetaChecker
import org.scalastyle.ScalastyleError

import scala.Array.fallbackCanBuildFrom
import scala.meta.tokens.Token

trait SpaceAroundTokenChecker extends CombinedMetaChecker {
  val DefaultTokens: String
  val disallowSpace: Boolean
  val beforeToken: Boolean

  // support old Scalariform token names, for backward compatibility
  private val map = Map(
    "LBRACKET" -> "[",
    "RBRACKET" -> "]",
    "LPAREN" -> "(",
    "RPAREN" -> ")",
    "LBRACE" -> "{",
    "RBRACE" -> "}",
    "EXCLAMATION" -> "!",
    "USCORE" -> "_",
    "COMMA" -> ",",
    "PLUS" -> "+",
    "DOT" -> ".",
    "HASH" -> "#",
    "MINUS" -> "-",
    "SEMI" -> ";",
    "COLON" -> ":",
    "IF" -> "if"
  )

  private def checkSpaces(t1: Token, t2: Token, t3: Token): Boolean = {
    // t1, t2, t3 are reversed if beforeToken is true
    // so we can have the same logic for both cases
    if (disallowSpace) {
      // the token directly after/before t1 cannot be a space
      SmVisitor.isA(t2, classOf[Token.Space])
    } else {
      // the token directly after/before t1 must be a space, but the token before/after that cannot be a space
      !SmVisitor.isA(t2, classOf[Token.Space]) || SmVisitor.isA(t3, classOf[Token.Space])
    }
  }

  def verify(ast: CombinedMeta): List[ScalastyleError] = {
    val tokens: Set[String] = getString("tokens", DefaultTokens).split(",").map(s => map.getOrElse(s.trim, s.trim)).toSet

    val it = for {
      (t1, t2, t3) <- SmVisitor.sliding3(ast.tree, reverse = beforeToken)
      if tokensContains(tokens, t1) && checkSpaces(t1, t2, t3)
    } yield {
      toError(t1, List(t1.text))
    }

    it.toList
  }

  private def tokensContains(tokens: Set[String], token: Token): Boolean = tokens.contains(token.text)
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
