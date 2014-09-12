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


import java.util.regex.Pattern

import org.scalastyle.{PositionError, ScalariformChecker, ScalastyleError}

import scalariform.lexer.Token
import scalariform.parser.CompilationUnit

class NonASCIICharacterChecker extends ScalariformChecker {
  val errorKey: String = "non.ascii.character.disallowed"

  override def verify(ast: CompilationUnit): List[ScalastyleError] = {
    ast.tokens.filter(hasNonAsciiChars).map(x => PositionError(x.offset))
  }

  private def hasNonAsciiChars(x: Token) =
    x.text.trim.nonEmpty && !Pattern.compile( """\p{ASCII}+""", Pattern.DOTALL)
      .matcher(x.text.trim).matches()

}
