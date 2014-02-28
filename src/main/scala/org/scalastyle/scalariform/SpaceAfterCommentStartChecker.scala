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
import org.scalastyle.PositionError
import org.scalastyle.ScalariformChecker
import org.scalastyle.ScalastyleError
import scalariform.lexer.MultiLineComment
import scalariform.lexer.ScalaDocComment
import scalariform.lexer.SingleLineComment
import scalariform.parser.CompilationUnit
import scalariform.lexer.Token

class SpaceAfterCommentStartChecker extends ScalariformChecker {
  val errorKey: String = "space.after.comment.start"

  val multiLinePattern = Pattern.compile("""/\*\S+.*""", Pattern.DOTALL)
  val scalaDocPattern = Pattern.compile("""/\*\*\S+.*""", Pattern.DOTALL)
  override def verify(ast: CompilationUnit): List[ScalastyleError] = {
    ast.tokens
      .filter(hasComment)
      .map {
      _.associatedWhitespaceAndComments.comments.map {
        case x: SingleLineComment if (x.token.text.trim.matches("""//\S+.*""")) => Some(x.token.offset)
        case x: MultiLineComment if (multiLinePattern.matcher(x.token.text.trim).matches()) => Some(x.token.offset)
        case x: ScalaDocComment if (scalaDocPattern.matcher(x.token.text.trim).matches()) => Some(x.token.offset)
        case _ => None
      }.flatten
    }.flatten.map(PositionError(_))
  }

  private def hasComment(x: Token) = x.associatedWhitespaceAndComments != null && !x.associatedWhitespaceAndComments.comments.isEmpty // scalastyle:ignore null
}
