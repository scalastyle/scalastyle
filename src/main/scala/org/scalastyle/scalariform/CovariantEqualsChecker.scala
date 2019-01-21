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

import org.scalastyle.Checker
import org.scalastyle.CombinedMeta
import org.scalastyle.CombinedMetaChecker
import org.scalastyle.ScalastyleError

import scala.meta.Defn
import scala.meta.Tree

class CovariantEqualsChecker extends CombinedMetaChecker {
  val errorKey = "covariant.equals"

  final def verify(ast: CombinedMeta): List[ScalastyleError] = {
    val classes = SmVisitor.getAll[Defn.Class](ast.tree)
    val objects = SmVisitor.getAll[Defn.Object](ast.tree)
    val traits = SmVisitor.getAll[Defn.Trait](ast.tree)

    val fs: List[Tree] = (classes ::: objects ::: traits).filter(matches).sortBy(_.pos.start)

    fs.map(d => toError(d))
  }

  private def matches(t: Tree): Boolean = {
    val equalsObject = exists(t, SmVisitor.isEqualsObject)
    val equalsOther = exists(t, isEqualsOther)

    !equalsObject && equalsOther
  }

  def exists(t: Tree, fn: Tree => Boolean): Boolean = {
    t match {
      case c: Defn.Class  => c.templ.children.exists(fn)
      case c: Defn.Object => c.templ.children.exists(fn)
      case c: Defn.Trait  => c.templ.children.exists(fn)
    }
  }

  private def isEqualsOther(t: Tree): Boolean = SmVisitor.matchMethod("equals", SmVisitor.singleParameter(Checker.isNotObject))(t)
}
