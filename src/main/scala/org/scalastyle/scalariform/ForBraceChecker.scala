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
import org.scalastyle.Lines
import org.scalastyle.ScalastyleError

import scala.meta.Term
import scala.meta.tokens.Token
import scala.meta.tokens.Tokens

class ForBraceChecker extends CombinedMetaChecker {
  val errorKey = "for.brace"

  val defaultSingleLineAllowed = false
  lazy val singleLineAllowed: Boolean = getBoolean("singleLineAllowed", defaultSingleLineAllowed)

  final def verify(ast: CombinedMeta): List[ScalastyleError] = {
    SmVisitor
      .getAll[Term.ForYield](ast.tree)
      .filter(t => !validSingleLine(t, ast.lines))
      .flatMap(t => hasBrace(t.tokens))
      .map(toError)
  }

  private def toIgnore(t: Token): Boolean = t match {
    case t: Token.Space   => true
    case t: Token.Tab     => true
    case t: Token.CR      => true
    case t: Token.LF      => true
    case t: Token.FF      => true
    case t: Token.Comment => true
    case t: Token.KwFor   => true
    case _                => false
  }

  private def hasBrace(ts: Tokens): Option[Token] = {
    ts.dropWhile(toIgnore).headOption.filterNot(t => SmVisitor.isA(t, classOf[Token.LeftBrace]))
  }

  private def validSingleLine(t: Term.ForYield, lines: Lines): Boolean = {
    singleLineAllowed && t.pos.startLine == t.pos.endLine
  }
}
