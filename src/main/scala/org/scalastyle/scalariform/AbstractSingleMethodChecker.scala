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

import scala.meta.Decl
import scala.meta.Defn
import scala.meta.Mod
import scala.meta.Stat
import scala.meta.Term
import scala.meta.Tree

sealed trait FullDefOrDclVisit {
  def fullDefOrDcl: Stat
  def name: Term.Name
  def insideDefOrValOrVar: Boolean
}

case class DefnDefVisit(d: Defn.Def, name: Term.Name, insideDefOrValOrVar: Boolean) extends FullDefOrDclVisit {
  def fullDefOrDcl: Stat = d
  def defnDef: Defn.Def = d
}

case class DeclDefVisit(d: Decl.Def, name: Term.Name, insideDefOrValOrVar: Boolean) extends FullDefOrDclVisit {
  def fullDefOrDcl: Stat = d
  def declDef: Decl.Def = d
}

abstract class AbstractSingleMethodChecker[T] extends CombinedMetaChecker {

  def verify(ast: CombinedMeta): List[ScalastyleError] = {
    val p = matchParameters()

    val defns = SmVisitor.getAll[Defn.Def](ast.tree).map(v => DefnDefVisit(v, v.name, inside(v.parent)))
    val decls = SmVisitor.getAll[Decl.Def](ast.tree).map(v => DeclDefVisit(v, v.name, inside(v.parent)))

    val fs: List[FullDefOrDclVisit] = (defns ::: decls).sortBy(_.name.pos.start)

    fs.filter(t => matches(t, p)).map(t => toError(t.name, describeParameters(p)))
  }

  protected def matchParameters(): T
  protected def matches(t: FullDefOrDclVisit, parameters: T): Boolean
  protected def describeParameters(parameters: T): List[String] = Nil

  private def inside(parent: Option[Tree]): Boolean = {
    parent.map {
      case f: Defn.Def => true
      case f: Defn.Var => true
      case f: Defn.Val => true
      case t: Tree        => inside(t.parent)
    }.getOrElse(false)
  }

  protected def isOverride(fullDefOrDclVisit: FullDefOrDclVisit): Boolean = {
    val mods = fullDefOrDclVisit match {
      case d: DeclDefVisit => d.d.mods
      case d: DefnDefVisit => d.d.mods
    }
    mods.exists {
      case sm: Mod.Override => true
      case _ => false
    }
  }

  protected def privateOrProtected(modifiers: List[Mod]): Boolean = modifiers.exists {
    case am: Mod.Private => true
    case am: Mod.Protected => true
    case _ => false
  }

  protected def params(t: FullDefOrDclVisit): List[List[Term.Param]] = t match {
    case d: DefnDefVisit => d.d.paramss
    case d: DeclDefVisit => d.d.paramss
  }

}
