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

package org.segl.scalastyle.scalariform

import java.lang.reflect.Constructor;
import scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.Tokens._
import org.segl.scalastyle.ScalariformChecker
import org.segl.scalastyle._
import org.segl.scalastyle.FileSpec

class ClassNamesChecker extends ScalariformChecker {
  val DefaultRegex = "[A-Z][A-Za-z]*"
  val errorKey = "class.name"

  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val regexString = getString("regex", DefaultRegex)
    val regex = regexString.r

    val it = for (
      List(left, right) <- ast.tokens.sliding(2);
      if (left.tokenType == CLASS && (regex findAllIn (right.getText)).size == 0)
    ) yield {
      PositionError(right.startIndex, List(regexString))
    }

    it.toList
  }
}

class ObjectNamesChecker extends ScalariformChecker {
  val DefaultRegex = "[A-Z][A-Za-z]*"
  val errorKey = "object.name"

  def verify(ast: CompilationUnit): List[PositionError] = {
    val regexString = getString("regex", DefaultRegex)
    val regex = regexString.r

    val it = for (
      List(left, right) <- ast.tokens.sliding(2);
      if (left.tokenType == OBJECT && (regex findAllIn (right.getText)).size == 0)
    ) yield {
      PositionError(right.startIndex, List(regexString))
    }

    it.toList
  }
}