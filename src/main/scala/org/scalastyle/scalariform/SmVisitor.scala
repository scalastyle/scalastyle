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

import scala.meta.Dialect
import scala.meta.Tree
import scala.meta.tokens.Token
import scala.meta.tokens.Tokens

object SmVisitor {
  class Clazz[+T <: Tree]()
  trait TreeVisit[T] {
    def subs: List[T]
  }

  protected[scalariform] def filterTokens[T <: Token](tokens: Tokens, matches: T => Boolean)(implicit manifest: Manifest[T]): Seq[T] = {
    for {
      t <- tokens
      if manifest.runtimeClass.isAssignableFrom(t.getClass)
      if matches(t.asInstanceOf[T])
    } yield t.asInstanceOf[T]
  }

  protected[scalariform] def traverse[T <: TreeVisit[T]](t: T, matches: T => Boolean): List[T] = {
    val l = t.subs.flatMap(traverse(_, matches))
    if (matches(t)) t :: l else l
  }

  protected[scalariform] def getAll[T <: Tree](ast: Tree)(implicit manifest: Manifest[T]): List[T] = {
    def fn(t: T): List[T] = List[T](t)

    visit0[T, T](manifest.runtimeClass.asInstanceOf[Class[T]], fn)(ast)
  }

  protected[scalariform] def visit[T <: Tree, X](fn: T => List[X])(ast: Any)(implicit manifest: Manifest[T]): List[X] = {
    visit0[T, X](manifest.runtimeClass.asInstanceOf[Class[T]], fn)(ast)
  }

  private[this] def visit0[T <: Tree, X](clazz: Class[T], fn: T => List[X])(ast: Any): List[X] = {
    val l = if (clazz.isAssignableFrom(ast.getClass)) {
      fn(ast.asInstanceOf[T])
    } else {
      Nil
    }

    l ::: visit(ast, visit0(clazz, fn))
  }

  protected[scalariform] def visit[T](ast: Any, visitfn: (Any) => List[T]): List[T] = {
    ast match {
      case a: Tree                   => visitfn(a.children)
      case Some(x)                   => visitfn(x)
      case xs @ (_ :: _)             => xs.flatMap(visitfn)
      case Left(x)                   => visitfn(x)
      case Right(x)                  => visitfn(x)
      case (l, r)                    => visitfn(l) ::: visitfn(r)
      case (x, y, z)                 => visitfn(x) ::: visitfn(y) ::: visitfn(z)
      case true | false | Nil | None => List()
    }
  }

  def charsBetweenTokens(left: Token, right: Token): Int = {
    println("right=" + right + " left=" + left + " " + (right.start ) + " " + left.end)
    right.start - left.end
  }

  def sliding2(tree: Tree)(implicit dialect: Dialect): Iterator[(Token, Token)] = {
    tree.tokens.sliding(2).map(l => (l(0), l(1)))
  }

  def sliding3(tree: Tree)(implicit dialect: Dialect): Iterator[(Token, Token, Token)] = {
    tree.tokens.sliding(3).map(l => (l(0), l(1), l(2)))
  }

  def sliding5(tree: Tree)(implicit dialect: Dialect): Iterator[(Token, Token, Token, Token, Token)] = {
    tree.tokens.sliding(5).map(l => (l(0), l(1), l(2), l(3), l(4))) // scalastyle:ignore magic.number
  }
}
