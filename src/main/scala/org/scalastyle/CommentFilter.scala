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

package org.scalastyle

import _root_.scalariform.lexer.HiddenTokenInfo
import _root_.scalariform.lexer.Tokens._
import scala.collection.mutable.ListBuffer

case class CommentFilter(start: Option[LineColumn], end: Option[LineColumn])

object CommentFilter {
  private case class CommentInter(position: Int, off: Boolean)

  private[this] def isOff(s: String) = s.contains("// scalastyle:off")

  def findCommentFilters(hiddenTokenInfo: HiddenTokenInfo, lines: Lines): List[CommentFilter] = {
    val it = for (
      hiddenTokens <- hiddenTokenInfo.allHiddenTokens;
      t <- hiddenTokens.rawTokens;
      if ((t.tokenType == LINE_COMMENT || t.tokenType == MULTILINE_COMMENT) && t.text.contains("// scalastyle:"))
    ) yield {
      CommentInter(t.startIndex, isOff(t.text))
    }

    val list = ListBuffer[CommentFilter]()
    var in: Boolean = false
    var start: Option[LineColumn] = None

    it.foreach(ci => {
      (in, ci.off) match {
        case (true, false) => { // off then on, add a new CommentFilter
          list += CommentFilter(start, lines.toLineColumn(ci.position))
          in = false
        }
        case (true, true) => // off then off, do nothing
        case (false, false) => // on then on, do nothing
        case (false, true) => { // on then off, reset start
          start = lines.toLineColumn(ci.position)
          in = true
        }
      }
    })

    if (in) {
      list += CommentFilter(start, None)
    }

    list.toList
  }

  def filterApplies[T <: FileSpec](m: Message[T], commentFilters: List[CommentFilter]): Boolean = {
    m match {
      case m: StyleError[T] => commentFilters.find(cf => gte(m.lineNumber, cf.start) && lte(m.lineNumber, cf.end)).isEmpty
      case _ => true
    }
  }

  def gte(line1: Option[Int], lineColumn: Option[LineColumn]) = line1.isEmpty || lineColumn.isEmpty || line1.get >= lineColumn.get.line

  def lte(line1: Option[Int], lineColumn: Option[LineColumn]) = line1.isEmpty || lineColumn.isEmpty || line1.get <= lineColumn.get.line
}