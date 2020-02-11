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

import _root_.scalariform.lexer.Tokens.INTERPOLATION_ID
import _root_.scalariform.parser.CompilationUnit
import org.scalastyle.{PositionError, ScalariformChecker, ScalastyleError}

class EmptyInterpolatedStringChecker extends ScalariformChecker {
  val errorKey = "empty.interpolated.strings"
  val interpolationRegex = """.*\$""".r
  val typesSupportingVariables = Set("s", "f")

  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val it = for {
      List(left, right) <- ast.tokens.sliding(2)
      if left.tokenType == INTERPOLATION_ID && typesSupportingVariables.contains(left.text) &&
      interpolationRegex.findFirstIn(right.text).isEmpty
    } yield {
      PositionError(right.offset)
    }

    it.toList
  }
}
