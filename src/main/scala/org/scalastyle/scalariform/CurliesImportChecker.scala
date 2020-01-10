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

import _root_.scalariform.parser.AstNode
import _root_.scalariform.parser.BlockImportExpr
import _root_.scalariform.parser.CompilationUnit
import _root_.scalariform.parser.ImportClause
import org.scalastyle.PositionError
import org.scalastyle.ScalariformChecker
import org.scalastyle.ScalastyleError

class CurliesImportChecker extends ScalariformChecker {

  val errorKey = "curlies.import"

  def verify(ast: CompilationUnit): List[ScalastyleError] =
    findBlockImports(ast)

  private def findBlockImports(in: AstNode): List[PositionError] = in match {

    // comma separated import
    case ImportClause(_, firstImport, otherImports, _) if otherImports.nonEmpty =>
      List(PositionError(firstImport.firstToken.offset))
    // other block imports
    case b: BlockImportExpr => List(PositionError(b.firstToken.offset))

    // remaining nodes
    case a: AstNode => a.immediateChildren flatMap findBlockImports
  }

}
