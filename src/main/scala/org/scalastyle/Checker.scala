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
import _root_.scalariform.lexer.HiddenTokenInfo
import _root_.scalariform.lexer.ScalaLexer
import _root_.scalariform.parser.ScalaParser
import _root_.scalariform.lexer.Token
import _root_.scalariform.lexer.Tokens._
import scala.io.Source

case class Line(text: String, start: Int, end: Int)

case class LineColumn(line: Int, column: Int)

case class Lines(lines: Array[Line]) {
  def toLineColumn(position: Int): Option[LineColumn] = {
    var i = 0

    lines.foreach(l => {
      i = i + 1
      if (position >= l.start && position < l.end) {
        return Some(LineColumn(i, position - l.start))
      }
    })

    None
  }
}

class ScalastyleChecker[T <: FileSpec] {
  def checkFiles(configuration: ScalastyleConfiguration, files: List[T]): List[Message[T]] = {
    val checks = configuration.checks.filter(_.enabled)
    StartWork() :: files.flatMap(file => StartFile(file) :: Checker.verifyFile(checks, file) ::: List(EndFile(file))).toList ::: List(EndWork())
  }
}

case class ScalariformAst(ast: CompilationUnit, hiddenTokenInfo: HiddenTokenInfo)

object Checker {
  type CheckerClass = Class[_ <: Checker[_]]

  def parseScalariform(source: String): Option[ScalariformAst] = {
    val (hiddenTokenInfo, tokens) = ScalaLexer.tokeniseFull(source, true)

    Some(ScalariformAst(new ScalaParser(tokens.toArray).compilationUnitOrScript(), hiddenTokenInfo))
  }

  def parseLines(source: String): Lines = Lines(source.split("\n").scanLeft(Line("", 0, 0)) {
          case (pl, t) => Line(t, pl.end, pl.end + t.length + 1)
        }.tail)

  def verifySource[T <: FileSpec](classes: List[ConfigurationChecker], file: T, source: String): List[Message[T]] = {
    val lines = parseLines(source)
    val scalariformAst = parseScalariform(source)

    val commentFilters = scalariformAst match {
      case Some(ast) => CommentFilter.findCommentFilters(ast.hiddenTokenInfo, lines)
      case None => List[CommentFilter]()
    }

    classes.flatMap(cc => newInstance(cc.className, cc.level, cc.parameters, cc.customMessage)).map(c => c match {
      case c: FileChecker => c.verify(file, c.level, lines, lines)
      case c: ScalariformChecker => scalariformAst match {
        case Some(ast) => c.verify(file, c.level, ast.ast, lines)
        case None => List[Message[T]]()
      }
      case c: CombinedChecker => scalariformAst match {
        case Some(ast) => c.verify(file, c.level, CombinedAst(ast.ast, lines), lines)
        case None => List[Message[T]]()
      }
      case _ => List[Message[T]]()
    }).flatten.filter(m => CommentFilter.filterApplies(m, commentFilters))
  }

  def verifyFile[T <: FileSpec](classes: List[ConfigurationChecker], file: T): List[Message[T]] = {
    try {
      val s = Source.fromFile(file.name).mkString
      verifySource(classes, file, s)
    } catch {
      case e: Exception => List(StyleException(file: T, None, message = e.getMessage(), stacktrace = e.getStackTraceString))
    }
  }

  def newInstance(name: String, level: Level, parameters: Map[String, String], customMessage: Option[String]): Option[Checker[_]] = {
    try {
      val clazz = Class.forName(name).asInstanceOf[Class[Checker[_]]]
      val c: Checker[_] = clazz.getConstructor().newInstance().asInstanceOf[Checker[_]]
      c.setParameters(parameters)
      c.setLevel(level)
      c.setCustomMessage(customMessage)
      Some(c)
    } catch {
      case e: Exception => {
        // TODO log something here
        None
      }
    }
  }
}

trait Checker[A] {
  val errorKey: String;
  var parameters = Map[String, String]();
  var level: Level = WarningLevel;
  var customMessage: Option[String] = None

  def setParameters(parameters: Map[String, String]) = this.parameters = parameters;
  def setLevel(level: Level) = this.level = level;
  def setCustomMessage(customMessage: Option[String]) = this.customMessage = customMessage
  def getInt(parameter: String, defaultValue: Int) = Integer.parseInt(parameters.getOrElse(parameter, "" + defaultValue))
  def getString(parameter: String, defaultValue: String) = parameters.getOrElse(parameter, defaultValue)
  def getBoolean(parameter: String, defaultValue: Boolean) = parameters.getOrElse(parameter, "" + defaultValue) == "true"

  protected def toStyleError[T <: FileSpec](file: T, p: ScalastyleError, level: Level, lines: Lines): Message[T] = {
    val p2 = p match {
      case PositionError(position, args) => {
        lines.toLineColumn(position) match {
          case Some(LineColumn(line, column)) => ColumnError(line, column, args)
          case None => FileError
        }
      }
      case _ => p
    }

    p2 match {
      case PositionError(position, args) => StyleError(file, this.getClass(), errorKey, level, args, customMessage = customMessage)
      case FileError(args) => StyleError(file, this.getClass(), errorKey, level, args, None, None, customMessage)
      case LineError(line, args) => StyleError(file, this.getClass(), errorKey, level, args, Some(line), None, customMessage)
      case ColumnError(line, column, args) => StyleError(file, this.getClass(), errorKey, level, args, Some(line), Some(column), customMessage)
    }
  }

  def charsBetweenTokens(left: Token, right: Token): Int = right.startIndex - (left.startIndex + left.length)

  def verify[T <: FileSpec](file: T, level: Level, ast: A, lines: Lines): List[Message[T]] = {
    verify(ast).map(p => toStyleError(file, p, level, lines))
  }

  def verify(ast: A): List[ScalastyleError]

  def isObject(s: String) = (s == "java.lang.Object" || s == "Any")
  def isNotObject(s: String) = !isObject(s)
}

trait FileChecker extends Checker[Lines]

trait ScalariformChecker extends Checker[CompilationUnit]

case class CombinedAst(compilationUnit: CompilationUnit, lines: Lines)

trait CombinedChecker extends Checker[CombinedAst]
