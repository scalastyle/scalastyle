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

package org.scalastyle.scalariform;

import java.lang.reflect.Constructor
import scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.Tokens._
import org.scalastyle.ScalariformChecker
import org.scalastyle._
import org.scalastyle.FileSpec
import _root_.scalariform.lexer.Token
import _root_.scalariform.parser.Refinement

class StructuralTypeChecker extends ScalariformChecker {
  val errorKey = "structural.type"
  import VisitorHelper._

  case class Position(position: Option[Int])

  final def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val it = for (
      f <- localvisit(ast.immediateChildren(0))
    ) yield {
      PositionError(f.position.get)
    }

    it.toList
  }

  private def localvisit(ast: Any): List[Position] = ast match {
    case t: Refinement => List(Position(Some(t.lbrace.offset)))
    case t: Any => visit(t, localvisit)
  }
}
