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

import java.lang.reflect.Constructor
import scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.Tokens._
import org.scalastyle.ScalariformChecker
import org.scalastyle._
import org.scalastyle.FileSpec
import _root_.scalariform.lexer.Token
import _root_.scalariform.parser.Refinement

class StructuralTypeChecker extends ScalariformChecker {
  val errorKey = "structural.type"

//  def verify(ast: CompilationUnit): List[ScalastyleError] = {
//    val it = for (
//      t <- ast.tokens;
//      if (isRefinement(t))
//    ) yield {
//      PositionError(t.startIndex)
//    }
//
//    it.toList
//  }

//  private[this] def isRefinement(t: Token) = {
//    t match {
//        case Refinement(a, b, c) => true
//        case _ => false
//      }
//  }
//
  import VisitorHelper._

  case class Position(position: Option[Int])

  final def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val it = for (
      f <- localvisit(ast.immediateChildren(0))
    ) yield {
      PositionError(f.position.get)
    }

    it.toList
  }

  private def localvisit(ast: Any): List[Position] = ast match {
    case t: Refinement => List(Position(Some(t.lbrace.startIndex)))
    case t: Any => visit(t, localvisit)
  }
//
//  protected def getParamTypes(pc: ParamClauses) = getParams(pc).map(p => typename(p.paramTypeOpt.get._2))
//
//  def matchFunDefOrDcl(t: BaseClazz[AstNode], fn: FunDefOrDcl => Boolean) = t match { case f: FunDefOrDclClazz => fn(f.t); case _ => false }
//
//  protected def methodMatch(name: String, paramTypesMatch: List[String] => Boolean)(t: FunDefOrDcl) =
//    t.nameToken.getText == name && paramTypesMatch(getParamTypes(t.paramClauses))
//
//  protected def singleParameter(fn: String => Boolean)(params: List[String]) = params.size == 1 && fn(params(0))
//  protected def noParameter()(params: List[String]) = params.size == 0
//  protected def isEqualsObject(t: FunDefOrDcl): Boolean = methodMatch("equals", singleParameter(isObject) _)(t)
//}
}
