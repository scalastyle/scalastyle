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

import scala.meta.tokens.Token

class NoWhitespaceBeforeLeftBracketChecker extends CombinedMetaChecker {
  val errorKey = "no.whitespace.before.left.bracket"

  def verify(ast: CombinedMeta): List[ScalastyleError] = {
    val it = for {
      (left, right) <- SmVisitor.sliding2(ast.tree)
      if left.getClass.isAssignableFrom(classOf[Token.Space]) && right.getClass.isAssignableFrom(classOf[Token.LeftBracket])
    } yield {
      toError(right)
    }

    it.toList
  }
}

class NoWhitespaceAfterLeftBracketChecker extends CombinedMetaChecker {
  val errorKey = "no.whitespace.after.left.bracket"

  def verify(ast: CombinedMeta): List[ScalastyleError] = {
    val it = for {
      (left, right) <- SmVisitor.sliding2(ast.tree)
      if left.getClass.isAssignableFrom(classOf[Token.LeftBracket]) && right.getClass.isAssignableFrom(classOf[Token.Space])
    } yield {
      toError(left)
    }

    it.toList
  }
}

class NoWhitespaceBeforeRightBracketChecker extends CombinedMetaChecker {
  val errorKey = "no.whitespace.before.right.bracket"

  def verify(ast: CombinedMeta): List[ScalastyleError] = {
    val it = for {
      (left, right) <- SmVisitor.sliding2(ast.tree)
      if left.getClass.isAssignableFrom(classOf[Token.Space]) && right.getClass.isAssignableFrom(classOf[Token.RightBracket])
    } yield {
      toError(right)
    }

    it.toList
  }
}
