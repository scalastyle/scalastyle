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

import _root_.scalariform.lexer.Comment
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class CommentFilter(id: Option[String], start: Option[LineColumn], end: Option[LineColumn])
case class CommentInter(id: Option[String], position: Int, off: Boolean)

object CommentFilter {

  private[this] val OnOff = """//\s*scalastyle:(on|off)(.*)""".r
  private[this] val OneLine = """//\s*scalastyle:ignore(.*)""".r
  private[this] val allMatchers = List(OnOff, OneLine)

  private[this] def isComment(s: String): Boolean = allMatchers.exists(_.pattern.matcher(s.trim).matches)

  def findScalastyleComments(tokens: List[Comment]): Iterable[Comment] = {
    tokens.filter(c => isComment(c.text))
  }

  def findCommentFilters(comments: List[Comment], lines: Lines): List[CommentFilter] =
    findOneLineCommentFilters(comments, lines) ++ findOnOffCommentFilters(comments, lines)

  private[this] def checkEmpty(s:String) = if (s != "") Some(s) else None
  private[this] def splitIds(s: String, notEmpty: Boolean = false):List[String] = s.trim.split("\\s+").toList match {
    case Nil if notEmpty => List("")
    case ls: List[String] => ls
  }

  private[this] def findOneLineCommentFilters(comments: List[Comment], lines: Lines):List[CommentFilter] =
    for {
      comment      <- comments
      OneLine(s)   <- List(comment.text.trim)
      (start, end) <- lines.toFullLineTuple(comment.token.offset).toList
      id           <- splitIds(s, true)
    } yield CommentFilter(checkEmpty(id), Some(start), Some(end))

  private[this] def findOnOffCommentFilters(comments: List[Comment], lines: Lines): List[CommentFilter] = {
    val it:List[CommentInter] =
      for {
        comment                <- comments
        OnOff(onoff, idString) <- List(comment.text.trim) // this is a bit ugly
        id                     <- splitIds( idString )
      } yield CommentInter( checkEmpty(id)
                          , comment.token.offset
                          , onoff == "off"
                          )

    val list = ListBuffer[CommentFilter]()
    val inMap = new mutable.HashMap[Option[String], Boolean]()
    val start = new mutable.HashMap[Option[String], Option[LineColumn]]()

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

  def filterApplies[T <: FileSpec](m: Message[T], cfs: List[CommentFilter]): Boolean = {
    m match {
      case se: StyleError[_] => !cfs.filter(idMatches(se.key)).exists(filterApplies(se))
      case _ => true
    }
  }

  private def idMatches(key: String)(cf: CommentFilter) = cf.id.isEmpty || cf.id.get == key

  private def filterApplies[T <: FileSpec](se: StyleError[_])(cf: CommentFilter): Boolean = {
    if (se.lineNumber.isEmpty) {
      true
    } else {
      val m = se.lineNumber.get
      (cf.start, cf.end) match {
        case (Some(s), Some(e)) => m >= s.line && m < e.line
        case (Some(s), None) => m >= s.line
        case (None, Some(e)) => false // we just have an :off, filter doesn't apply
        case (None, None) => false // this isn't possible, say it doesn't apply
      }
    }
  }
}
