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
import scalariform.parser.AnonymousFunction
import scalariform.parser.AstNode
import scalariform.parser.Expr
import scalariform.parser.CompilationUnit
import org.scalastyle.PositionError
import scalariform.parser.MatchExpr
import scalariform.parser.CallExpr

class RemovableMatchChecker extends ScalariformChecker {
  val errorKey = "removable.match"
  val targetCalls = Set("compose", "count", "dropWhile", "exists", "filter", "filterNot", "find", "flatMap",
    "forall", "foreach", "groupBy", "indexWhere", "lastIndexWhere", "map", "mapConserve", "maxBy", "minBy",
    "partition", "prefixLength", "reverseMap", "segmentLength", "sortBy", "span", "takeWhile", "withFilter")

  final def verify(ast: CompilationUnit): List[ScalastyleError] = {
    getAllCallExpr(ast).withFilter(t => targetCalls.contains(t.id.text) && hasRemovableMatch(t))
      .map(t => PositionError(t.id.offset))
      .toList
  }

  private def hasRemovableMatch(c: CallExpr): Boolean = {
    val anonymousFunction = findAnonymousFunction(c.newLineOptsAndArgumentExprss(0)._2)

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

  private def findAnonymousFunction(ast: AstNode, level: Int = 0): Option[AnonymousFunction] = {
    if (level > 3) {
      return None
    }
    ast.immediateChildren.headOption match {
      case Some(n: AnonymousFunction) => Some(n)
      case Some(n) => findAnonymousFunction(n)
      case _ => None
    }
  }

  private def getAllCallExpr(ast: Any): List[CallExpr] = ast match {
    case t: CallExpr => t :: visit(t.immediateChildren, getAllCallExpr)
    case t: Any => visit(t, getAllCallExpr)
  }
}
