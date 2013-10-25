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

import org.scalastyle.FileError
import org.scalastyle.ScalariformChecker
import org.scalastyle.ScalastyleError

import scalariform.parser.CompilationUnit
import scalariform.parser.TmplDef
import VisitorHelper.visit

class NumberOfTypesChecker extends ScalariformChecker {
  val errorKey = "number.of.types"
  val DefaultMaximumTypes = 30

  final def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val maximumTypes = getInt("maxTypes", DefaultMaximumTypes)

    val it = for {
      f <- localvisit(ast.immediateChildren(0))
    } yield {
      f
    }

    if (it.size > maximumTypes) List(FileError(List(maximumTypes.toString))) else List()
  }

  private def localvisit(ast: Any): List[TmplDef] = ast match {
    case t: TmplDef => List(t) ::: localvisit(t.templateBodyOption)
    case t: Any => visit(t, localvisit)
  }
}
