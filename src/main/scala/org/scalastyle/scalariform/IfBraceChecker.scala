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

import org.scalastyle.CombinedMeta
import org.scalastyle.CombinedMetaChecker
import org.scalastyle.Lines
import org.scalastyle.ScalastyleError
import org.scalastyle.scalariform.SmVisitor.TreeVisit

import scala.meta.Term

class IfBraceChecker extends CombinedMetaChecker {
  val DefaultSingleLineAllowed = true
  val DefaultDoubleLineAllowed = false
  val errorKey = "if.brace"

  def verify(ast: CombinedMeta): List[ScalastyleError] = {
    val doubleLineAllowed = getBoolean("doubleLineAllowed", DefaultDoubleLineAllowed)
    val singleLineAllowed = doubleLineAllowed || getBoolean("singleLineAllowed", DefaultSingleLineAllowed)

    val it = for {
      t <- localvisit(ast.tree)
      f <- traverse(t, ast.lines, singleLineAllowed, doubleLineAllowed)
    } yield {
      toError(f.t)
    }

    it
  }

  case class IfExprClazz(t: Term.If, body: List[IfExprClazz], elseClause: List[IfExprClazz]) extends TreeVisit[IfExprClazz] {
    def subs: List[IfExprClazz] = body ::: elseClause
  }

  private def traverse(t: IfExprClazz, lines: Lines, singleLineAllowed: Boolean, doubleLineAllowed: Boolean): List[IfExprClazz] = {
    val l = t.subs.flatMap(traverse(_, lines, singleLineAllowed, doubleLineAllowed))
    if (matches(t, lines, singleLineAllowed, doubleLineAllowed)) t :: l else l
  }

  def matches(t: IfExprClazz, lines: Lines, singleLineAllowed: Boolean, doubleLineAllowed: Boolean): Boolean = {
    val ifLine: Int = t.t.pos.startLine
    val ifThenLine: Int = t.t.thenp.pos.startLine
    val ifHasBrace = hasBrace(t.t.thenp)

    val (elseLine: Option[Int], elseHasBrace) = t.t.elsep match {
      case t: Term => (Some(t.pos.startLine), hasBrace(t))
      case _       => (None, true)
    }

    if (ifHasBrace && elseHasBrace) {
      false
    } else {
      elseLine match {
        case None => if (singleLineAllowed) !sameLine(ifLine, Some(ifThenLine)) else true
        case Some(y) => {
          if (!sameLine(ifLine, Some(ifThenLine)) || !sameLine(y, elseLine)) {
            true
          } else {
            if (sameLine(ifLine, elseLine)) !singleLineAllowed else !doubleLineAllowed
          }
        }
      }
    }
  }

  private[this] def sameLine(l1: Int, l2: Option[Int]) = l2 match {
    case Some(y) => l1 == y
    case None    => true
  }

  private[this] def hasBrace(term: Term): Boolean = {
    term match {
      case b: Term.Block => true
      case i: Term.If    => true
      case _             => term.toString().startsWith("{")
    }
  }

  private def localvisit(ast: Any): List[IfExprClazz] = ast match {
    case t: Term.If => List(IfExprClazz(t, localvisit(t.thenp), localvisit(t.elsep)))
    case t: Any     => SmVisitor.visit(t, localvisit)
  }
}
