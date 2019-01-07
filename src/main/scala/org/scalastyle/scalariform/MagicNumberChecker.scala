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

import scala.meta.Defn
import scala.meta.Lit
import scala.meta.Term

class MagicNumberChecker extends CombinedMetaChecker {
  val DefaultIgnore = "-1,0,1,2"
  val errorKey = "magic.number"

  def verify(ast: CombinedMeta): List[ScalastyleError] = {
    val ignores = getString("ignore", DefaultIgnore).split(",").map(_.trim).toSet

    // get unary pluses
    val unaryList: List[Term.ApplyUnary] = SmVisitor.getAll[Term.ApplyUnary](ast.tree).filter(isUnaryPlus(ignores))
    val litsFromUnaryList = unaryList.map(_.arg).toSet

    // get int & long literals, but remove ignored ones & ones which appear in unary pluses above
    val intList: List[Lit] = SmVisitor.getAll[Lit.Int](ast.tree).filterNot(ignoredLit(ignores, litsFromUnaryList))
    val longList: List[Lit] = SmVisitor.getAll[Lit.Long](ast.tree).filterNot(ignoredLit(ignores, litsFromUnaryList))

    val all = (intList ::: longList ::: unaryList).sortBy(_.pos.start)

    // all rhs of vals
    val valRhsSet = SmVisitor.getAll[Defn.Val](ast.tree).map(_.rhs).toSet

    // remove all terms which appear in rhs of val decl
    all.filterNot(t => valRhsSet.contains(t)).map(toError)
  }

  private def ignoredLit(ignores: Set[String], unaryLits: Set[Term])(l: Lit): Boolean = ignores.contains(l.value.toString) || unaryLits.contains(l)

  private def isUnaryPlus(ignores: Set[String])(au: Term.ApplyUnary): Boolean = au match {
    case Term.ApplyUnary(Term.Name("+"), i: Lit.Int) => !ignores.contains("" + i.value)
    case Term.ApplyUnary(Term.Name("+"), i: Lit.Long) => !ignores.contains("" + i.value)
    case _ => false
  }
}
