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

import scala.meta.Lit
import scala.meta.Term

class SimplifyBooleanExpressionChecker extends CombinedMetaChecker {
  val errorKey = "simplify.boolean.expression"
  private val infixOps = Set("!=", "==", "&&", "||")

  def verify(ast: CombinedMeta): List[ScalastyleError] = {
    val it1: List[Term] = SmVisitor.getAll[Term.ApplyUnary](ast.tree)

    val it2 = SmVisitor.getAll[Term.ApplyInfix](ast.tree)

    (it1 ::: it2).filter(matches).sortWith((a, b) => a.pos.start < b.pos.start).map(toError)
  }

  private def matches(t: Term): Boolean = {
    t match {
      case t: Term.ApplyInfix => matchesInfixOp(t) && (boolean(t.lhs) || boolean(t.args.head))
      case t: Term.ApplyUnary => matchesUnaryOp(t) && boolean(t.arg)
      case _                  => false
    }
  }

  private def matchesInfixOp(t: Term.ApplyInfix) = infixOps.contains(t.op.value)
  private def matchesUnaryOp(t: Term.ApplyUnary) = t.op.value == "!"

  private def boolean(t: Term) = t match {
    case b: Lit.Boolean => true
    case _              => false
  }
}
