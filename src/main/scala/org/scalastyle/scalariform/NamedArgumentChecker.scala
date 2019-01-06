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

import org.scalastyle._

import scala.meta.Lit
import scala.meta.Term
import scala.util.matching.Regex

/**
  * Checks method calls to ensure that passed literals are named.
  */
class NamedArgumentChecker extends CombinedMetaChecker {
  protected val errorKey: String = "named.argument"
  val DefaultCheckString = false
  val DefaultIgnoreMethod = "^set.+$"

  override def verify(ast: CombinedMeta): List[ScalastyleError] = {
    val checkString = getBoolean("checkString", DefaultCheckString)
    val ignoreMethod = getString("ignoreMethod", DefaultIgnoreMethod)

    for {
      a <- SmVisitor.getAll[Term.Apply](ast.tree)
      arg <- matchingArguments(a, checkString, ignoreMethod.r)
    } yield {
      toError(arg)
    }
  }

  private def matchingArguments(a: Term.Apply, checkString: Boolean, ignoreMethod: Regex): List[Term] = {
    if (ignoreMethod.findFirstIn(a.fun.toString).isDefined) {
      Nil
    } else {
      a.args.filter(argMatches(checkString))
    }
  }

  private def argMatches(checkString: Boolean)(term: Term): Boolean = term match {
    case Term.Assign(lhs: Term.Name, _) => false
    case l: Lit                         => true
    case x: Term.Interpolate            => checkString
    case _                              => false
  }
}
