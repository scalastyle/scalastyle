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

import _root_.scalariform.parser.CompilationUnit
import _root_.scalariform.parser.TmplDef
import org.scalastyle.PositionError
import org.scalastyle.ScalariformChecker
import org.scalastyle.ScalastyleError
import org.scalastyle.scalariform.VisitorHelper.TreeVisit
import org.scalastyle.scalariform.VisitorHelper.traverse
import org.scalastyle.scalariform.VisitorHelper.visit

abstract class AbstractClassChecker extends ScalariformChecker {
  case class TmplClazz(t: TmplDef, subs: List[TmplClazz]) extends TreeVisit[TmplClazz]

  final def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val it = for {
      f <- visit[TmplDef, TmplClazz](map)(ast.immediateChildren.head)
      t <- traverse(f, matches)
    } yield {
      PositionError(t.t.name.offset)
    }

    it
  }

  def matches(t: TmplClazz): Boolean

  private def map(t: TmplDef): List[TmplClazz] = List(TmplClazz(t, visit(map)(t.templateBodyOption)))
}
