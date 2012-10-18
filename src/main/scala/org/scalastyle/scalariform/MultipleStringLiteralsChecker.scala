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
import scalariform.parser.CompilationUnit
import scalariform.lexer.Tokens.STRING_LITERAL

class MultipleStringLiteralsChecker extends ScalariformChecker {
  private val DefaultAllowed = 1
  val errorKey = "multiple.string.literals"
  private val MultiQuote = "\"\"\""
  private val MultiQuoteLength = MultiQuote.length

  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val allowed = getInt("allowed", DefaultAllowed)

    val ts = ast.tokens.filter(t => t.tokenType == STRING_LITERAL).groupBy(t => strip(t.text))
    ts.filter(g => g._2.size > allowed).map(g => PositionError(g._2(0).offset, List(g._1, "" + g._2.size, "" + allowed))).toList
  }

  private def strip(s: String) = if (startsAndEndsWith(s, MultiQuote)) "\"" + s.substring(MultiQuoteLength,s.length()-MultiQuoteLength) + "\"" else s

  private def startsAndEndsWith(s: String, sufpre: String) = s.startsWith(sufpre) && s.endsWith(sufpre)
}
