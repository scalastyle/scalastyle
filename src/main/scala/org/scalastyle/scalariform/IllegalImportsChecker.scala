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

import scalariform.parser.CompilationUnit
import scalariform.parser.ImportExpr
import scalariform.parser.BlockImportExpr
import scalariform.parser.ImportSelectors
import scalariform.parser.ImportClause
import _root_.scalariform.lexer.Tokens._
import _root_.scalariform.lexer.Token
import org.scalastyle.ScalariformChecker
import org.scalastyle.PositionError
import org.scalastyle.ScalastyleError
import scala.collection.mutable.ListBuffer

class IllegalImportsChecker extends ScalariformChecker {
  import VisitorHelper._

  val errorKey = "illegal.imports"

  case class Import(position: Int, importString: String)
  case class State(state: String)
  val ExpectingImport = State("expectingImport")
  val InImport = State("inImport")

  val DefaultIllegalImports = "sun._"

  // sun._ => sun\.
  // sun.com.foobar => sun\.com\.foobar
  private def toMatchList(s: String) = {
    s.trim().split(" *, *").map(s => s.replaceAll("_$", "")).toList
  }

  case class ImportClauseVisit(t: ImportClause, importExpr: List[ImportClauseVisit], otherImportExprs: List[ImportClauseVisit]);

  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    var illegalImportsList = toMatchList(getString("illegalImports", DefaultIllegalImports))

    val it = for (
      t <- localvisit(ast.immediateChildren);
      f <- traverse(t, illegalImportsList)
    ) yield {
      PositionError(t.t.firstToken.offset)
    }

    it.toList
  }

  private def traverse(t: ImportClauseVisit, illegalImportsList: List[String]): List[ImportClauseVisit] = {
    val l = t.importExpr.map(traverse(_, illegalImportsList)).flatten ::: t.otherImportExprs.map(traverse(_, illegalImportsList)).flatten
    if (matches(t, illegalImportsList)) t :: l else l
  }

  private[this] def imports(tokens: List[Token]): String = {
    tokens.foldLeft("")((a, b) => a + b.text)
  }

  private[this] def imports(t: BlockImportExpr): List[String] = {
    val is = t.importSelectors

    val firsts = is.firstImportSelector.firstToken.text ::
            is.otherImportSelectors.map(_._2).map(is => is.firstToken.text)
    firsts.map(f => imports(t.prefixExpr.tokens) + f)
  }

  private[this] def matches(t: ImportClauseVisit, illegalImportsList: List[String]): Boolean = {
    val list = t.t.importExpr match {
      case t: BlockImportExpr => imports(t)
      case _ => List(imports(t.t.importExpr.tokens))
    }

    illegalImportsList.exists(ill => list.exists(s => s.startsWith(ill)))
  }

  private[this] def localvisit(ast: Any): List[ImportClauseVisit] = ast match {
    case t: ImportClause => List(ImportClauseVisit(t, localvisit(t.importExpr), localvisit(t.otherImportExprs)))
    case t: Any => visit(t, localvisit)
  }
}
