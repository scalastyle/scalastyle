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
import scalariform.parser.CompilationUnit
import scalariform.parser.Refinement
import scalariform.parser.ForExpr
import scalariform.lexer.TokenType
import scalariform.lexer.TokenType
import scalariform.lexer.Tokens

class ForBraceChecker extends ScalariformChecker {
  val errorKey = "for.brace"

  final def verify(ast: CompilationUnit): List[ScalastyleError] = {
    for {
      t <- VisitorHelper.getAll[ForExpr](ast.immediateChildren(0))
      if (Tokens.LPAREN == t.lParenOrBrace.tokenType || Tokens.LPAREN == t.rParenOrBrace.tokenType)
    } yield PositionError(t.lParenOrBrace.offset)
  }
}
