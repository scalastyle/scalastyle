package org.segl.scalastyle.scalariform

import java.lang.reflect.Constructor;
import scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.Tokens._
import _root_.scalariform.lexer.Token
import org.segl.scalastyle.ScalariformChecker
import org.segl.scalastyle._
import scala.collection.mutable.ListBuffer
import org.segl.scalastyle._

// TODO deal with alias and multiple imports, i.e: import java.util.{List => JList} import java.util.{List, Map}
class IllegalImportsChecker extends ScalariformChecker {
  val errorKey = "illegal.imports"

  case class Import(position: Int, importString: String)
  case class State(state: String)
  val ExpectingImport = State("expectingImport")
  val InImport = State("inImport")

  val DefaultillegalImports = "sun._,sun.com.foobar"

  // sun._ => sun\.
  // sun.com.foobar => sun\.com\.foobar
  def toMatchList(s: String) = {
    s.split(",").map(s => s.replaceAll("_$", "")).toList
  }

  def getImports(ast: CompilationUnit): List[Import] = {
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
    var illegalImportsList = toMatchList(getString("illegalImports", DefaultillegalImports))
    val it = for (
      importedClass <- getImports(ast);
      if (illegalImportsList.exists(importedClass.importString.startsWith(_)))
    ) yield {
      PositionError(importedClass.position)
    }

    it.toList
  }
}