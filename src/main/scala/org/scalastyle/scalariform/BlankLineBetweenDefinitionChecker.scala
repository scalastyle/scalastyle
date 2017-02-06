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

import _root_.scalariform.parser._
import org.scalastyle.{CombinedChecker, ScalastyleError, CombinedAst, Lines, PositionError}
import org.scalastyle.scalariform.VisitorHelper.{TreeVisit, visit}
import scala.annotation.tailrec

/**
 * According to the Effective Scala "http://twitter.github.io/effectivescala/index.html#Formatting-Whitespace"
 * they suggest, Use one blank line between method, class, and object definitions.
 *
 * this checker detects class/object/method where not contains one blank line between definition.
 *
 * Configuration.
 * to allow no blank line between class/object and method, like below
 *
 * class Foo{
 * def bar = ...
 * }
 *
 * to allow this option, set parameter "true" at configuration xml
 * by default its set to "false"
 *
 * <parameters>
 * <parameter name="NoBlankAfterClassAllowed">true</parameter>
 * </parameters>
 */
class BlankLineBetweenDefinitionChecker extends CombinedChecker {
  val errorKey = "blankline.between.definition"

  val paramNoBlankAfterClassAllowed = "NoBlankAfterClassAllowed"
  val paramNoBlankAfterClassAllowedDefValue = false

  case class TmplClazz(t: TmplDef, subs: List[TmplClazz]) extends TreeVisit[TmplClazz]

  private def map(t: TmplDef): List[TmplClazz] = List(TmplClazz(t, visit(map)(t.templateBodyOption)))

  final def verify(ast: CombinedAst): List[ScalastyleError] = {
    val noBlankAfterClassAllowed = getBoolean(paramNoBlankAfterClassAllowed, paramNoBlankAfterClassAllowedDefValue)

    val cu = ast.compilationUnit
    val clazz = visit[TmplDef, TmplClazz](map)(cu.immediateChildren(0));

    clazz.map(verifyClazz(_, ast.lines, noBlankAfterClassAllowed)).flatten
  }

  private def verifyClazz(t: TmplClazz, lines: Lines,
                          noBlankAfterClassAllowed: Boolean): List[ScalastyleError] = t.t.templateBodyOption match {
    case Some(b)
    => verifyBody(b, lines, noBlankAfterClassAllowed) ::: t.subs.map(verifyClazz(_, lines, noBlankAfterClassAllowed)).flatten
    case None => Nil
  }

  private def verifyBody(b: TemplateBody, lines: Lines,
                         noBlankAfterClassAllowed: Boolean): List[ScalastyleError] = {
    val ss = b.statSeq
    val head = lines.toLineColumn(b.firstToken.offset).get.line

    val otherStats = for (
      (t, Some(s)) <- ss.otherStats
    ) yield s

    val stats = ss.firstStatOpt match {
      case Some(s) => s :: otherStats
      case None => otherStats
    }

    val statsWithCodeRange = for (
      s <- stats;
      r <- toCodeRanges(s, lines)
    ) yield (s, r)

    val defOrDclsWithCodeRange = statsWithCodeRange.filter {
      case (x: FullDefOrDcl, _) if x.defOrDcl.isInstanceOf[FunDefOrDcl] || x.defOrDcl.isInstanceOf[TmplDef] => true
      case _ => false
    }

    val results = for (
      d <- defOrDclsWithCodeRange;
      a <- check(d, statsWithCodeRange, lines, head, noBlankAfterClassAllowed)
    ) yield a

    results
  }

  private def check(t: (Stat, (Int, Int)), cs: List[(Stat, (Int, Int))],
                    lines: Lines, head: Int, noBlankAfterClassAllowed: Boolean): Option[ScalastyleError] = {
    val (funDef, (lineNumber, _)) = t

    val exprList = for (c <- cs if c._2._2 != lineNumber) yield c._2._2
    val exprListWithClassDef = if (noBlankAfterClassAllowed) {
      exprList
    } else {
      head :: exprList
    }

    funDef.firstTokenOption.flatMap(t => {
      if (checkIsBlankLine(lineNumber, lines, exprListWithClassDef)) {
        None
      } else {
        Some(PositionError(t.offset))
      }
    })
  }

  final val BlockCommentStart = """/\*""".r
  final val BlockCommentFinish = """\*/""".r

  final val LineNumber2ArrayAdjuster = 1

  @tailrec
  private def checkIsBlankLine(lineNumber: Int, source: Lines, endOfExprList: List[Int]): Boolean = {
    if (!endOfExprList.contains(lineNumber)) {
      val skip = skipComment(lineNumber, source)
      if (skip == lineNumber) true else checkIsBlankLine(skip, source, endOfExprList)
    } else {
      false
    }
  }

  private def skipComment(lineNumber: Int, source: Lines): Int = {
    val text = source.lines(lineNumber - LineNumber2ArrayAdjuster).text
    if (text.length != 0) {
      execSkipComment(lineNumber, source, 0)
    } else {
      lineNumber
    }
  }

  @tailrec
  private def execSkipComment(lineNumber: Int, source: Lines, blockCommentLevel: Int): Int = {
    val text = source.lines(lineNumber - LineNumber2ArrayAdjuster).text
    val diff = blockCommentLevel + BlockCommentFinish.findAllIn(text).length - BlockCommentStart.findAllIn(text).length
    if (diff == 0) {
      lineNumber - 1
    } else {
      execSkipComment(lineNumber - 1, source, diff)
    }
  }

  private def toCodeRanges(stats: Stat, lines: Lines): Option[(Int, Int)] = {
    val offsets = (stats.firstTokenOption, stats.lastTokenOption) match {
      case (Some(s), Some(e)) => Some((s.offset, e.offset))
      case _ => None
    }

    for ((s, e) <- offsets;
         sLine <- lines.toLineColumn(s);
         eLine <- lines.toLineColumn(e)
    ) yield (sLine.line, eLine.line)
  }
}
