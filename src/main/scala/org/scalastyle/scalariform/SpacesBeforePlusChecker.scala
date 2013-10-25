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

import scalariform.lexer.Tokens.LBRACKET
import scalariform.lexer.Tokens.NEWLINE
import scalariform.lexer.Tokens.PLUS
import scalariform.parser.CompilationUnit

class SpacesBeforePlusChecker extends ScalariformChecker {
  val errorKey = "spaces.before.plus"

  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val it = for {
      List(left, middle, right) <- ast.tokens.sliding(3);
      if (middle.tokenType == PLUS && left.tokenType != LBRACKET && left.tokenType != NEWLINE && charsBetweenTokens(left, middle) == 0)
    } yield {
      PositionError(middle.offset)
    }

    it.toList
  }
}
