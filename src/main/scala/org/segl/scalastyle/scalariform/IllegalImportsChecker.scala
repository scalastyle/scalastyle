package org.segl.scalastyle.scalariform

import java.lang.reflect.Constructor;
import scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.Tokens._
import _root_.scalariform.lexer.Token
import org.segl.scalastyle.ScalariformChecker
import org.segl.scalastyle._
import scala.collection.mutable.ListBuffer

class IllegalImportsChecker extends ScalariformChecker {
  case class Import(position: Int, importString: String)
  case class State(state: String)
  val ExpectingImport = State("expectingImport") 
  val InImport = State("inImport")
  
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
    
    return list.toList
  }
  
  def verify(file: String, ast: CompilationUnit): List[Message] = {
    println("imports=" + getImports(ast))
    val it = for (
      importedClass <- getImports(ast);
      // TODO don't just check sun._
      if (importedClass.importString == "sun._")
    ) yield {
      StyleError(file, "illegal.imports", position = Some(importedClass.position))
    }

    return it.toList
  }
}