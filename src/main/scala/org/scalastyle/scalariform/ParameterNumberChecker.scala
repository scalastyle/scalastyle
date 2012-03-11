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

import java.lang.reflect.Constructor;
import scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.Tokens._
import _root_.scalariform.lexer.Token
import _root_.scalariform.parser._
import org.scalastyle.ScalariformChecker
import org.scalastyle._

class ParameterNumberChecker extends ScalariformChecker {
  val errorKey = "parameter.number"

  import VisitorHelper._
  val DefaultMaximumParameters = 8

  case class FunDefOrDclClazz(paramClauses: ParamClauses, position: Option[Int], subs: List[FunDefOrDclClazz]) extends Clazz[FunDefOrDcl]()

  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val maxParameters = getInt("maxParameters", DefaultMaximumParameters)

    val it = for (
      t <- localvisit(ast.immediateChildren(0));
      f <- traverse(t);
      if (matches(f, maxParameters))
    ) yield {
      PositionError(t.position.get, List("" + maxParameters))
    }

    it.toList
  }

  private def traverse(t: FunDefOrDclClazz): List[FunDefOrDclClazz] = t :: t.subs.map(traverse(_)).flatten

  private def matches(t: FunDefOrDclClazz, maximumParameters: Int) = getParams(t.paramClauses).size > maximumParameters

  private def getParams(p: ParamClauses): List[Param] = {
    p.paramClausesAndNewlines.map(_._1).flatMap(pc => pc.firstParamOption :: pc.otherParams.map(p => Some(p._2))).flatten
  }

  private def typename(t: Type): String = t.tokens.map(_.getText).mkString

  private def localvisit(ast: Any): List[FunDefOrDclClazz] = ast match {
    case t: FunDefOrDcl => List(FunDefOrDclClazz(t.paramClauses, Some(t.nameToken.startIndex), localvisit(t.localDef)))
    case t: Any => visit(t, localvisit)
  }
}