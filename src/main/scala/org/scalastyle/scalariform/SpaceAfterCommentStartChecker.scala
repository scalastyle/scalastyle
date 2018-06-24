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

import org.scalastyle.ScalametaChecker
import org.scalastyle.ScalastyleError

import scala.meta.Tree
import scala.meta.tokens.Token

class SpaceAfterCommentStartChecker extends ScalametaChecker {
  private val MLC1 = Pattern.compile("""/\*\S+.*""", Pattern.DOTALL)
  private val MLC2 = Pattern.compile("""/\*.*\S\*/""", Pattern.DOTALL)
  private val SD1 = Pattern.compile("""/\*\*\S+.*""", Pattern.DOTALL)
  private val SD2 = Pattern.compile("""/\*\*.*\S\*/""", Pattern.DOTALL)
  private val SLC1 = Pattern.compile("""//\S+.*""")
  private val SLC2 = Pattern.compile("""///+""")

  val errorKey: String = "space.after.comment.start"

  override def verify(ast: Tree): List[ScalastyleError] = {
    val it = for {
      t <- ast.tokens.tokens
      if t.isInstanceOf[Token.Comment]
      if matches(t.asInstanceOf[Token.Comment])
    } yield toError(t.asInstanceOf[Token.Comment])

    it.toList
  }

  private def matches(c: Token.Comment): Boolean = {
    val text = c.text.trim
    if (text.startsWith("/**")) {
      scalaDocPatternRegex(text)
    } else {
      multiLineCommentRegex(text) || singleLineCommentRegex(text)
    }
  }

  private def multiLineCommentRegex(s: String): Boolean = MLC1.matcher(s).matches() || MLC2.matcher(s).matches()
  private def scalaDocPatternRegex(text: String): Boolean = SD1.matcher(text).matches() || SD2.matcher(text).matches()
  private def singleLineCommentRegex(s: String): Boolean = SLC1.matcher(s).matches() && !SLC2.matcher(s).matches()
}
