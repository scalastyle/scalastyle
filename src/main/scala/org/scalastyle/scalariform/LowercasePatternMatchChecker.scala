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

import org.scalastyle.FileError
import org.scalastyle.ScalariformChecker
import org.scalastyle.ScalastyleError
import org.scalastyle.PositionError

import scalariform.parser.CompilationUnit
import scalariform.parser.TmplDef
import scalariform.parser.CasePattern
import scalariform.parser.Expr
import scalariform.parser.GeneralTokens
import scalariform.lexer.Token
import scalariform.lexer.Tokens.VARID
import VisitorHelper.visit

class LowercasePatternMatchChecker extends ScalariformChecker {
  val errorKey = "lowercase.pattern.match"

  final def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val it = for {
      f <- visit(map)(ast.immediateChildren(0));
      if (matches(f))
    } yield {
      PositionError(f.pattern.firstToken.offset)
    }

    it.toList
  }

  private def matches(t: CasePattern) = {
    t.pattern.tokens match {
      case List(t: Token) => (t.tokenType == VARID && t.text.length() > 0 && t.text(0).isLower)
      case _ => false
    }
  }

  private def map(t: CasePattern): List[CasePattern] = List(t) ::: visit(map)(t.pattern) ::: visit(map)(t.guardOption)
}
