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
import org.scalastyle.scalariform.SmVisitor.Clazz

import scala.meta.Defn
import scala.meta.tokens.Token
import scala.meta.tokens.Token.Ident

class CyclomaticComplexityChecker extends CombinedMetaChecker {
  val errorKey = "cyclomatic.complexity"
  val DefaultMaximum = 10
  val DefaultCountCases = true
  private lazy val maximum = getInt("maximum", DefaultMaximum)
  private lazy val countCases = getBoolean("countCases", DefaultCountCases)
  private val defaultTokens: Set[Class[_ <: Token]] = Set(classOf[Token.KwIf], classOf[Token.KwWhile], classOf[Token.KwDo], classOf[Token.KwFor])

  case class FunDefOrDclClazz(t: Defn.Def, subs: List[FunDefOrDclClazz]) extends Clazz[Defn.Def]()

  def verify(ast: CombinedMeta): List[ScalastyleError] = {
    val it = for {
      t <- localvisit(ast.tree.children.head)
      f <- traverse(t)
      value = matches(f, ast.lines, maximum)
      if value > maximum
    } yield {
      toError(t.t.name, List("" + value, "" + maximum))
    }

    it
  }

  private def traverse(t: FunDefOrDclClazz): List[FunDefOrDclClazz] = t :: t.subs.flatMap(traverse)

  private def isLogicalOrAnd(t: Token): Boolean = t.getClass == classOf[Ident] && (t.text == "&&" || t.text == "||")

  // compute the cyclomatic complexity without the additional 1
  private def cyclomaticComplexity(f: FunDefOrDclClazz): Int = {
    val tokens = defaultTokens + (if (countCases) classOf[Token.KwCase] else classOf[Token.KwMatch])
    getAllTokens[Token](f.t).count(t => tokens.contains(t.getClass) || isLogicalOrAnd(t))
  }

  private def matches(t: FunDefOrDclClazz, lines: Lines, maxLines: Int) = {
    val root = cyclomaticComplexity(t)
    val subs = t.subs.map(cyclomaticComplexity).sum
    root - subs + 1
  }

  private def localvisit(ast: Any): List[FunDefOrDclClazz] = ast match {
    case t: Defn.Def => List(FunDefOrDclClazz(t, SmVisitor.visit(t, localvisit)))
    case t: Any => SmVisitor.visit(t, localvisit)
  }
}
