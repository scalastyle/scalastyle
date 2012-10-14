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

import org.scalastyle.PositionError
import org.scalastyle.ScalariformChecker
import org.scalastyle.ScalastyleError

import VisitorHelper.visit
import scalariform.lexer.Tokens.LBRACE
import scalariform.lexer.Tokens.RBRACE
import scalariform.parser.AstNode
import scalariform.parser.CompilationUnit
import scalariform.parser.TmplDef

class EmptyClassChecker extends ScalariformChecker {
  val errorKey = "empty.class"
  import VisitorHelper._

  case class TmplClazz(t: TmplDef, position: Option[Int], subs: List[TmplClazz])

  final def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val it = for (
      f <- localvisit(ast.immediateChildren(0));
      t <- traverse(f)
    ) yield {
      PositionError(t.position.get)
    }

    it.toList
  }

  private def traverse(t: TmplClazz): List[TmplClazz] = {
    val l = t.subs.map(traverse(_)).flatten
    if (matches(t)) t :: l else l
  }

  private def isEmptyBlock(ast: AstNode): Boolean = {
    ast.tokens.size == 2 && ast.tokens(0).tokenType == LBRACE && ast.tokens(1).tokenType == RBRACE
  }

  def matches(t: TmplClazz): Boolean = {
    t.t.templateBodyOption match {
      case None => false
      case Some(tbo) => isEmptyBlock(tbo)
    }
  }

  private def localvisit(ast: Any): List[TmplClazz] = ast match {
    case t: TmplDef => List(TmplClazz(t, Some(t.name.offset), localvisit(t.templateBodyOption)))
    case t: Any => visit(t, localvisit)
  }
}
