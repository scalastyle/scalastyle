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

import scalariform.lexer.Tokens
import scalariform.parser.CompilationUnit
import org.scalastyle._

class SingleSpaceAfterKeyword extends ScalariformChecker {
  override protected val errorKey: String = "single.space.after.keyword"

  override def verify(ast: CompilationUnit): List[ScalastyleError] = {
    (for {
      List(left, right) <- ast.tokens.sliding(2)
      if Tokens.KEYWORDS.contains(left.tokenType) && charsBetweenTokens(left, right) != 1
    } yield {
      PositionError(right.offset)
    }).toList
  }
}
