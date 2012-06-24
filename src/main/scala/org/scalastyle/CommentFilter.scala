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
import _root_.scalariform.lexer.Comment
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap

case class CommentFilter(id: Option[String], start: Option[LineColumn], end: Option[LineColumn])
case class CommentInter(id: Option[String], position: Int, off: Boolean)

object CommentFilter {

  private[this] val MatchRegex = """//\s*scalastyle:(on|off)(.*)""".r

  def isComment(s: String): Boolean = MatchRegex.pattern.matcher(s.trim).matches

  def findScalastyleComments(hiddenTokenInfo: HiddenTokenInfo): Iterable[Comment] = {
    hiddenTokenInfo.allHiddenTokens.map(hiddenTokens => hiddenTokens.comments).flatten.filter(c => isComment(c.getText))
  }

  def findCommentFilters(hiddenTokenInfo: HiddenTokenInfo, lines: Lines): List[CommentFilter] = {
    val comments = findScalastyleComments(hiddenTokenInfo)

    val it = comments.map(c => {
      c.getText.trim match {
        case MatchRegex(onoff, idString) => {
          val ids = idString.trim.split("\\s+")
          val off = onoff == "off"
          ids.map(id => CommentInter(if (id != "") Some(id) else None, c.token.startIndex, off)).toList
        }
        case _ => List() // shouldn't get here
      }
    }).flatten.toList.sortWith((e1, e2) => (e1.position - e2.position) < 0)

    val list = ListBuffer[CommentFilter]()
    var inMap = new HashMap[Option[String], Boolean]()
    var start = new HashMap[Option[String], Option[LineColumn]]()

    it.foreach(ci => {
      (inMap.getOrElse(ci.id, false), ci.off) match {
        case (true, false) => { // off then on, add a new CommentFilter
          list += CommentFilter(ci.id, start.getOrElse(ci.id, None), lines.toLineColumn(ci.position))
          inMap.put(ci.id, false)
          start.remove(ci.id)
        }
        case (true, true) => // off then off, do nothing
        case (false, false) => // on then on, do nothing
        case (false, true) => { // on then off, reset start
          start.put(ci.id, lines.toLineColumn(ci.position))
          inMap.put(ci.id, true)
        }
      }
    })

    inMap.foreach( e => {
      if (e._2) {
        list += CommentFilter(e._1, start.getOrElse(e._1, None), None)
      }
    })

    list.toList
  }

  def filterApplies[T <: FileSpec](m: Message[T], commentFilters: List[CommentFilter]): Boolean = {
    m match {
      case m: StyleError[_] => {
        val filters = commentFilters.filter(cf => !cf.id.isDefined || cf.id.get == m.key)
        filters.find(cf => gte(m.lineNumber, cf.start) && lte(m.lineNumber, cf.end)).isEmpty
      }
      case _ => true
    }
  }

  protected def gte(line1: Option[Int], lineColumn: Option[LineColumn]) = line1.isEmpty || lineColumn.isEmpty || line1.get >= lineColumn.get.line

  protected def lte(line1: Option[Int], lineColumn: Option[LineColumn]) = line1.isEmpty || lineColumn.isEmpty || line1.get <= lineColumn.get.line
}
