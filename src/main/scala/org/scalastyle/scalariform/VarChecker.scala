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

import org.scalastyle.PositionError
import org.scalastyle.ScalariformChecker
import org.scalastyle.ScalastyleError

import scalariform.lexer.Tokens.VAR
import scalariform.parser.AnonymousFunction
import scalariform.parser.CompilationUnit
import scalariform.parser.FunBody
import scalariform.parser.PatDefOrDcl
import scalariform.parser.TemplateBody
import VisitorHelper.visit

abstract class VarChecker extends ScalariformChecker {
  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val it = for {
      f <- localvisit(false)(ast.immediateChildren(0))
    } yield {
      PositionError(f.firstToken.offset)
    }

    it.toList
  }

  protected def matches(enclosingFunction: Boolean): Boolean

  private def localvisit(enclosingFunction: Boolean)(ast: Any): List[PatDefOrDcl] = ast match {
    case t: PatDefOrDcl if t.valOrVarToken.tokenType == VAR && matches(enclosingFunction) => List(t) ::: visit(t, localvisit(enclosingFunction))
    case t: TemplateBody => visit(t, localvisit(false))
    case t: FunBody => visit(t, localvisit(true))
    case t: AnonymousFunction => visit(t, localvisit(true))
    case t: Any => visit(t, localvisit(enclosingFunction))
  }
}

class VarLocalChecker extends VarChecker {
  val errorKey = "var.local"
  override protected def matches(enclosingFunction: Boolean): Boolean = enclosingFunction
}

class VarFieldChecker extends VarChecker {
  val errorKey = "var.field"
  override protected def matches(enclosingFunction: Boolean): Boolean = !enclosingFunction
}
