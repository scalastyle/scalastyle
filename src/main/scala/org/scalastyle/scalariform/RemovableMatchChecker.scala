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

import org.scalastyle.ScalariformChecker
import org.scalastyle.ScalastyleError
import org.scalastyle.scalariform.VisitorHelper._
import scalariform.parser._
import org.scalastyle.PositionError
import scalariform.parser.AnonymousFunction
import scalariform.parser.CompilationUnit
import org.scalastyle.PositionError
import scala.Some
import scalariform.parser.MatchExpr
import scalariform.parser.Expr
import scalariform.parser.CallExpr

class RemovableMatchChecker extends ScalariformChecker {
  val errorKey = "removable.match"
  val targetCalls = Set("compose", "count", "dropWhile", "exists", "filter", "filterNot", "find", "flatMap",
    "forall", "foreach", "groupBy", "indexWhere", "lastIndexWhere", "map", "mapConserve", "maxBy", "minBy",
    "partition", "prefixLength", "reverseMap", "segmentLength", "sortBy", "span", "takeWhile", "withFilter")

  final def verify(ast: CompilationUnit): List[ScalastyleError] = {
    getAllCallAndInfixExpr(ast).withFilter(isTargetDefinitions).map(getErrorPosition)
  }

  private def isTargetDefinitions(t: ExprElement): Boolean = t match {
    case t: CallExpr => targetCalls.contains(t.id.text) && hasRemovableMatch(t)
    case t: InfixExpr =>
      findAnonymousFunction(t.right.head) match {
        case Some(f) => targetCalls.contains(t.infixId.text) && isRemovable(f)
        case None => false
      }
  }

  private def getErrorPosition(t: ExprElement): PositionError = t match {
    case t: CallExpr => PositionError(t.id.offset)
    case t: InfixExpr => PositionError(t.infixId.offset)
  }

  private def hasRemovableMatch(c: CallExpr): Boolean = {
    val anonymousFunction = c.newLineOptsAndArgumentExprss match {
      case x::_ => findAnonymousFunction(x._2)
      case _ => None
    }

    anonymousFunction match {
      case Some(f) => isRemovable(f)
      case None => false
    }
  }

  private def isRemovable(f: AnonymousFunction): Boolean = f.body.firstStatOpt match {
    case Some(Expr(List(m: MatchExpr))) =>
      val param = f.firstToken.text
      val id = m.left.head.asInstanceOf[CallExpr].id
      if (param == id.text) {
        !m.block.tokens.exists(_.text == param)
      } else {
        false
      }
    case _ => false
  }

  private def findAnonymousFunction(ast: AstNode, level: Int = 0): Option[AnonymousFunction] =
    ast.immediateChildren.headOption match {
      case opt if level > 3 || opt.isEmpty => None
      case Some(n: AnonymousFunction) => Some(n)
      case Some(n) => findAnonymousFunction(n)
    }

  private def getAllCallAndInfixExpr(ast: Any): List[ExprElement] = ast match {
    case t: InfixExpr => t :: visit(t.immediateChildren, getAllCallAndInfixExpr)
    case t: CallExpr => t :: visit(t.immediateChildren, getAllCallAndInfixExpr)
    case t: Any => visit(t, getAllCallAndInfixExpr)
  }
}
