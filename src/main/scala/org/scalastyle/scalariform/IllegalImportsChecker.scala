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

import java.lang.reflect.Constructor;
import scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.Tokens._
import _root_.scalariform.lexer.Token
import org.scalastyle.ScalariformChecker
import org.scalastyle._
import scala.collection.mutable.ListBuffer
import org.scalastyle._

// TODO deal with alias and multiple imports, i.e: import java.util.{List => JList} import java.util.{List, Map}
class IllegalImportsChecker extends ScalariformChecker {
  val errorKey = "illegal.imports"

  case class Import(position: Int, importString: String)
  case class State(state: String)
  val ExpectingImport = State("expectingImport")
  val InImport = State("inImport")

  val DefaultIllegalImports = "sun._"

  // sun._ => sun\.
  // sun.com.foobar => sun\.com\.foobar
  private def toMatchList(s: String) = {
    s.split(",").map(s => s.replaceAll("_$", "")).toList
  }

  private def getImports(ast: CompilationUnit): List[Import] = {
    val list = ListBuffer[Import]()
    var position = 0;
    val current = new StringBuilder()
    var state = ExpectingImport

    ast.tokens.foreach(token => {
      state match {
        case ExpectingImport => if (token.tokenType == IMPORT) {
          state = InImport
          position = token.startIndex
        }
        case InImport => {
          if (token.tokenType == NEWLINE || token.tokenType == NEWLINES || token.tokenType == SEMI) {
            state = ExpectingImport
            list += Import(position, current.toString)
            position = 0;
            current.clear()
          } else {
            current.append(token.text)
          }
        }
      }
    })

    if (state == InImport) {
      list += Import(position, current.toString)
    }

    list.toList
  }

  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    var illegalImportsList = toMatchList(getString("illegalImports", DefaultIllegalImports))
    val it = for (
      importedClass <- getImports(ast);
      if (illegalImportsList.exists(importedClass.importString.startsWith(_)))
    ) yield {
      PositionError(importedClass.position)
    }

    it.toList
  }
}
