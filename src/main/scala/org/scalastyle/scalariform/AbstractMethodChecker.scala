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

import _root_.scalariform.lexer.Token
import _root_.scalariform.parser.AstNode
import org.scalastyle.CombinedMeta
import org.scalastyle.CombinedMetaChecker
import org.scalastyle.ScalastyleError

import scala.meta.Decl
import scala.meta.Defn
import scala.meta.Tree

object VisitorHelper {
  class Clazz[+T <: AstNode]()
  trait TreeVisit[T] {
    def subs: List[T]
  }

  protected[scalariform] def traverse[T <: TreeVisit[T]](t: T, matches: T => Boolean): List[T] = {
    val l = t.subs.flatMap(traverse(_, matches))
    if (matches(t)) t :: l else l
  }

  protected[scalariform] def getAllRecursive[T <: AstNode](ast: Any)(implicit manifest: Manifest[T]): List[T] = {
    def fn(t : T): List[T] = List[T](t) ++ t.immediateChildren.flatMap(child => getAllRecursive[T](child))

    myVisit[T, T](manifest.runtimeClass.asInstanceOf[Class[T]], fn)(ast)
  }

  protected[scalariform] def getAll[T <: AstNode](ast: Any)(implicit manifest: Manifest[T]): List[T] = {
    def fn(t : T): List[T] = List[T](t)

    myVisit[T, T](manifest.runtimeClass.asInstanceOf[Class[T]], fn)(ast)
  }

  protected[scalariform] def visit[T <: AstNode, X](fn: T => List[X])(ast: Any)(implicit manifest: Manifest[T]): List[X] = {
    myVisit[T, X](manifest.runtimeClass.asInstanceOf[Class[T]], fn)(ast)
  }

  private[this] def myVisit[T <: AstNode, X](clazz: Class[T], fn: T => List[X])(ast: Any): List[X] = {
    if (ast.getClass.equals(clazz)) {
      fn(ast.asInstanceOf[T])
    } else {
      visit(ast, myVisit(clazz, fn))
    }
  }

  protected[scalariform] def visit[T](ast: Any, visitfn: (Any) => List[T]): List[T] = ast match {
    case a: AstNode => visitfn(a.immediateChildren)
    case t: Token => List()
    case Some(x) => visitfn(x)
    case xs @ (_ :: _) => xs.flatMap(visitfn(_))
    case Left(x) => visitfn(x)
    case Right(x) => visitfn(x)
    case (l, r) => visitfn(l) ::: visitfn(r)
    case (x, y, z) => visitfn(x) ::: visitfn(y) ::: visitfn(z)
    case true | false | Nil | None => List()
  }
}

abstract class AbstractMethodChecker extends CombinedMetaChecker {
  final def verify(ast: CombinedMeta): List[ScalastyleError] = {
    val defns = SmVisitor.getAll[Defn.Def](ast.tree)
    val decls = SmVisitor.getAll[Decl.Def](ast.tree)

    val fs: List[Tree] = (defns ::: decls).filter(matches).sortBy(_.name.pos.start)

    fs.map(toError)
  }

  def matches(t: Tree): Boolean
}
