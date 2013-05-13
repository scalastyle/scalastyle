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

import org.scalastyle.{ ScalastyleError, ScalariformChecker, PositionError }

import scalariform.lexer.Tokens.VARID
import scalariform.parser.CompilationUnit

class NotImplementedErrorUsage extends ScalariformChecker {

  val errorKey = "not.implemented.error.usage"

  def verify(ast: CompilationUnit): List[ScalastyleError] =
    for {
      t <- ast.tokens
      if t.tokenType == VARID && t.text == "???"
    } yield PositionError(t.offset)
}
