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

import org.scalastyle.PositionError
import org.scalastyle.ScalariformChecker
import org.scalastyle.ScalastyleError

import scalariform.parser.{Annotation => ParserAnnotation}
import scalariform.parser.CompilationUnit

class DeprecatedJavaChecker extends ScalariformChecker {
  val errorKey = "deprecated.java"
  val deprecatedTokens = List("Deprecated", "java.lang.Deprecated")

  final def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val it = for {
      t <- VisitorHelper.getAll[ParserAnnotation](ast.immediateChildren(0));
      if (isDeprecated(t))
    } yield {
      PositionError(t.firstToken.offset)
    }

    it.toList
  }

  private def isDeprecated(t: ParserAnnotation) = {
    val text = t.annotationType.tokens.foldLeft("")((x, y) => x + y.text)
    t.annotationType.tokens.size > 0 && deprecatedTokens.contains(text)
  }
}
