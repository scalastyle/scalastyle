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
import scala.util.matching.Regex

abstract class AbstractTokenChecker[T <: Token](val errorKey: String, tokenType: Class[T]) extends CombinedMetaChecker {
  def verify(ast: CombinedMeta): List[ScalastyleError] = {
    val it = for {
      t <- ast.tree.tokens
      if t.getClass.isAssignableFrom(tokenType) && matches(t)
    } yield {
      toError(t)
    }

    it.toList
  }

  protected def matches(token: Token): Boolean = true
}

class UppercaseLChecker extends AbstractTokenChecker("uppercase.l", classOf[Token.Constant.Long]) {
  override def matches(t: Token): Boolean = t.text.endsWith("l")
}

class WhileChecker extends AbstractTokenChecker("while", classOf[Token.KwWhile])
class ReturnChecker extends AbstractTokenChecker("return", classOf[Token.KwReturn])

class TokenChecker extends AbstractTokenChecker("token", classOf[Token.Ident]) {
  private val DefaultRegex = "^$"
  lazy val regex: Regex = getString("regex", DefaultRegex).r

  override protected def matches(t: Token): Boolean = regex.findFirstIn(t.text).isDefined
}
