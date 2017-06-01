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

import org.scalastyle.CombinedAst
import org.scalastyle.CombinedChecker
import org.scalastyle.Lines
import org.scalastyle.PositionError
import org.scalastyle.ScalastyleError
import org.scalastyle.scalariform.VisitorHelper.Clazz
import org.scalastyle.scalariform.VisitorHelper.visit

import _root_.scalariform.parser.FunDefOrDcl

class MethodLengthChecker extends CombinedChecker {
  val errorKey = "method.length"
  val DefaultMaximumLength = 50
  val DefaultIgnoreComments = false

  private val SinglelineComment = "//"
  private val MultilineCommentsOpener = "/*"
  private val MultilineCommentsCloser = "*/"

  case class FunDefOrDclClazz(t: FunDefOrDcl, position: Option[Int], subs: List[FunDefOrDclClazz]) extends Clazz[FunDefOrDcl]()

  def verify(ast: CombinedAst): List[ScalastyleError] = {
    val maxLength = getInt("maxLength", DefaultMaximumLength)
    val ignoreComments = getBoolean("ignoreComments", DefaultIgnoreComments)

    val it = for {
      t <- localvisit(ast.compilationUnit.immediateChildren.head)
      f <- traverse(t)
      if matches(f, ast.lines, maxLength, ignoreComments)
    } yield {
      PositionError(t.position.get, List("" + maxLength))
    }

    it
  }

  private def traverse(t: FunDefOrDclClazz): List[FunDefOrDclClazz] = t :: t.subs.flatMap(traverse)

  private def matches(t: FunDefOrDclClazz, lines: Lines, maxLines: Int, ignoreComments: Boolean) = {
    if (ignoreComments) {
      val count = for {
        (_, start) <- lines.findLineAndIndex(t.t.defToken.offset)
        (_, end) <- lines.findLineAndIndex(t.t.tokens.last.offset)
      } yield {
        var count = 0
        var multilineComment = false

        // do not count deftoken line and end block line
        for (i <- (start + 1) until (end - 1)) {
          val lineText = lines.lines(i).text.trim
          if (lineText.startsWith(SinglelineComment)) {
            // do nothing
          } else {
            if (lineText.contains(MultilineCommentsOpener)) {
              // multiline comment start /*
              // this line won't be counted even if
              // there exists any token before /*
              multilineComment = true
            }
            if (!multilineComment) {
              count = count + 1
            }
            if (lineText.contains(MultilineCommentsCloser)) {
              // multiline comment end */
              // this line won't be counted even if
              // there exists any token after */
              multilineComment = false
            }
          }
        }
        count
      }
      count.get > maxLines
    } else {
      val head = lines.toLineColumn(t.t.defToken.offset)
      val tail = lines.toLineColumn(t.t.tokens.last.offset)
      tail.get.line - head.get.line > maxLines
    }
  }

  private def localvisit(ast: Any): List[FunDefOrDclClazz] = ast match {
    case t: FunDefOrDcl => List(FunDefOrDclClazz(t, Some(t.nameToken.offset), localvisit(t.localDef)))
    case t: Any => visit(t, localvisit)
  }
}
