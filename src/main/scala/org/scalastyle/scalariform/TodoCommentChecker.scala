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

import _root_.scalariform.lexer.Tokens
import org.scalastyle.CombinedAst
import org.scalastyle.CombinedChecker
import org.scalastyle.PositionError
import org.scalastyle.ScalastyleError

/**
 * comment check for line comment style TODO or FIXME
 */
class TodoCommentChecker extends CombinedChecker {
  val errorKey = "todo.comment"
  val defaultWords = "TODO|FIXME"

  def verify(ast: CombinedAst): List[ScalastyleError] = {
    val words = getString("words", defaultWords)
    val split = words.split("\\|").map(Pattern.quote).mkString("|")
    val regex = ("""(?i)(//|/\*|/\*\*|\*)\s?(""" + split + """)(:?)\s+""").r

    for {
      t  <- ast.compilationUnit.tokens
      at <- t.associatedWhitespaceAndComments
      if Tokens.COMMENTS.contains(at.token.tokenType)
      if at.text.split("\n").exists(s => regex.findFirstIn(s).isDefined)
    } yield PositionError(at.token.offset, List(words))
  }
}
