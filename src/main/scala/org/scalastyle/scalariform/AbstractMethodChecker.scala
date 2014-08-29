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

package org.scalastyle.scalariform;

import _root_.scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.Token
import org.scalastyle.ScalariformChecker
import scalariform.parser.ParamClauses
import scalariform.parser.TmplDef
import org.scalastyle.ScalastyleError
import scalariform.parser.FunDefOrDcl
import scalariform.parser.Param
import scalariform.parser.Type
import org.scalastyle.PositionError
import scalariform.parser.AstNode
import VisitorHelper.{visit, Clazz}

object VisitorHelper {
  class Clazz[+T <: AstNode]()
  trait TreeVisit[T] {
    def subs: List[T]
  }

  protected[scalariform] def traverse[T <: TreeVisit[T]](t: T, matches: T => Boolean): List[T] = {
    val l = t.subs.map(traverse(_, matches)).flatten
    if (matches(t)) t :: l else l
  }

  protected[scalariform] def getAll[T <: AstNode](ast: Any)(implicit manifest: Manifest[T]): List[T] = {
    def fn(t : T): List[T] = List[T](t)

    myVisit[T, T](manifest.runtimeClass.asInstanceOf[Class[T]], fn)(ast)
  }

  protected[scalariform] def visit[T <: AstNode, X](fn: T => List[X])(ast: Any)(implicit manifest: Manifest[T]): List[X] = {
    myVisit[T, X](manifest.runtimeClass.asInstanceOf[Class[T]], fn)(ast)
  }

  private[this] def myVisit[T <: AstNode, X](clazz: Class[T], fn: T => List[X])(ast: Any): List[X] = {
    if (ast.getClass().equals(clazz)) {
      fn(ast.asInstanceOf[T])
    } else {
      visit(ast, myVisit(clazz, fn))
    }
  }

  protected[scalariform] def visit[T](ast: Any, visitfn: (Any) => List[T]): List[T] = ast match {
    case a: AstNode => visitfn(a.immediateChildren)
    case t: Token => List()
    case Some(x) => visitfn(x)
    case xs @ (_ :: _) => xs flatMap { visitfn(_) }
    case Left(x) => visitfn(x)
    case Right(x) => visitfn(x)
    case (l, r) => visitfn(l) ::: visitfn(r)
    case (x, y, z) => visitfn(x) ::: visitfn(y) ::: visitfn(z)
    case true | false | Nil | None => List()
  }
}

abstract class AbstractMethodChecker extends ScalariformChecker {
  type ListType = List[BaseClazz[_ <: AstNode]]
  protected def params(t: BaseClazz[AstNode]): List[String] = List()

  class BaseClazz[+T <: AstNode](val t: T, val position: Option[Int], val subs: ListType) extends Clazz[T] {
    def is(fn: T => Boolean): Boolean = false
  }

  case class TmplClazz(_t: TmplDef, _position: Option[Int], _subs: ListType) extends BaseClazz[TmplDef](_t, _position, _subs)
  case class FunDefOrDclClazz(_t: FunDefOrDcl, _position: Option[Int], _subs: ListType) extends BaseClazz[FunDefOrDcl](_t, _position, _subs) {
    override def is(fn: FunDefOrDcl => Boolean): Boolean = fn(this.t)
  }

  final def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val it = for {
      t <- localvisit(ast.immediateChildren(0));
      f <- traverse(t);
      if (matches(f))
    } yield {
      PositionError(f.position.get, params(f))
    }

    it.toList
  }

  private def traverse(t: BaseClazz[AstNode]): ListType = {
    val l = t.subs.map(traverse(_)).flatten
    if (matches(t)) t :: l else l
  }

  def matches(t: BaseClazz[AstNode]): Boolean

  protected def getParams(p: ParamClauses): List[Param] = {
    p.paramClausesAndNewlines.map(_._1).flatMap(pc => pc.firstParamOption :: pc.otherParams.map(p => Some(p._2))).flatten
  }

  protected def typename(t: Type): String = t.tokens.map(_.text).mkString

  private def localvisit(ast: Any): ListType = ast match {
    case t: TmplDef => List(TmplClazz(t, Some(t.name.offset), localvisit(t.templateBodyOption)))
    case t: FunDefOrDcl => List(FunDefOrDclClazz(t, Some(t.nameToken.offset), localvisit(t.localDef)))
    case t: Any => visit(t, localvisit)
  }

  protected def getParamTypes(pc: ParamClauses) = getParams(pc).map(p => typename(p.paramTypeOpt.get._2))

  protected def matchFunDefOrDcl(t: BaseClazz[AstNode], fn: FunDefOrDcl => Boolean) = t match { case f: FunDefOrDclClazz => fn(f.t); case _ => false }

  protected def methodMatch(name: String, paramTypesMatch: List[String] => Boolean)(t: FunDefOrDcl) =
    t.nameToken.text == name && paramTypesMatch(getParamTypes(t.paramClauses))

  protected def singleParameter(fn: String => Boolean)(params: List[String]) = params.size == 1 && fn(params(0))
  protected def noParameter()(params: List[String]) = params.size == 0
  protected def isEqualsObject(t: FunDefOrDcl): Boolean = methodMatch("equals", singleParameter(isObject) _)(t)
}

