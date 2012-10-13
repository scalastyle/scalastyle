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
import _root_.scalariform.lexer.Token
import _root_.scalariform.parser.Refinement
import  _root_.scalariform.parser.{Annotation => ParserAnnotation}

class DeprecatedJavaChecker extends ScalariformChecker {
  val errorKey = "deprecated.java"
  import VisitorHelper._
  val deprecatedTokens = List("Deprecated", "java.lang.Deprecated")

  case class Position(position: Option[Int])

  final def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val it = for (
      f <- localvisit(ast.immediateChildren(0))
    ) yield {
      PositionError(f.position.get)
    }

    it.toList
  }

  private def isDeprecated(t: ParserAnnotation) = {
    val text = t.annotationType.tokens.foldLeft("")((x, y) => x + y.text)
    t.annotationType.tokens.size > 0 && deprecatedTokens.contains(text)
  }

  private def localvisit(ast: Any): List[Position] = ast match {
    case t: ParserAnnotation if (isDeprecated(t)) => List(Position(Some(t.firstToken.offset)))
    case t: Any => visit(t, localvisit)
  }
}
