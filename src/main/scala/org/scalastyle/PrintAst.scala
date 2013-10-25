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

package org.scalastyle
import _root_.scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.Token

// scalastyle:off regex

object PrintAst {
  def main(args: Array[String]): Unit = {
    val source = """package foobar
class Foobar {
  def foobar() = {
    val f1 = 5
    val f2 = 5 :: Nil

    println("it=" + it.toList)
  }
}
"""

    printAst(source)
  }

  def printAst(source: String): Unit =  {
    val lines = Checker.parseLines(source)
    val scalariformAst = Checker.parseScalariform(source)
    scalariformAst match {
      case None => println("Parse error")
      case Some(ast) => printAst(lines, ast.ast)
    }
  }

  private def lineNumber(lines: Lines, token: Token) = lines.toLineColumn(token.offset)

  private def printAst(lines: Lines, ast: CompilationUnit): Unit = {
    val lineMap = ast.tokens.groupBy(t => lines.toLineColumn(t.offset).get.line)

    val lineNumbers = lineMap.keys.toList.sortWith((a, b) => a < b)
    lineNumbers.foreach(ln => println(("%02d" format ln) + ": " + lineMap.get(ln).get))

    println("ast=" + ast)
  }
}
