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

import org.scalastyle.ScalariformChecker
import org.scalastyle.ScalastyleError

import org.scalastyle.scalariform.VisitorHelper.visit
import scalariform.parser.Generator
import scalariform.parser.CompilationUnit
import org.scalastyle.PositionError
import scalariform.parser.Expr
import scalariform.lexer.{Tokens, Token}

class GeneratorVariableLengthChecker extends ScalariformChecker {
  val DefaultMaxVariableLength = 1
  val errorKey = "generator.variable.length"

  final def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val maxVariableLength = getInt("maxVariableLength", DefaultMaxVariableLength)

    val it = for (
      t <- VisitorHelper.getAll[Generator](ast.immediateChildren(0));
      f <- localvisit(t.pattern)
      if (isTooLong(f, maxVariableLength))
    ) yield {
      PositionError(f.offset, List(maxVariableLength.toString()))
    }

    it.toList
  }

  private def isTooLong(t: Token, maxLength: Int): Boolean = {
    t.text.length > maxLength
  }

  private def localvisit(ast: Any): List[Token] = ast match {
    case t: Expr =>
      if (t.contents.length == 1 && t.firstToken.tokenType == Tokens.VARID) {
        t.firstToken :: localvisit(t.immediateChildren(0))
      } else {
        localvisit(t.contents)
      }
    case t: Any => visit(t, localvisit)
  }
}
