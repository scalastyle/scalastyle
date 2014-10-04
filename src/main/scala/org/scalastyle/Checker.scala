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
import _root_.scalariform.lexer.HiddenToken
import _root_.scalariform.lexer.ScalaLexer
import _root_.scalariform.lexer.Comment
import _root_.scalariform.parser.ScalaParser
import _root_.scalariform.lexer.Token
import scala.io.Source
import java.nio.charset.MalformedInputException
import scala.io.Codec
import scala.collection.JavaConversions.seqAsJavaList
import scala.collection.JavaConversions.collectionAsScalaIterable

case class Line(text: String, start: Int, end: Int)

case class HiddenTokenInfo(tokens: Seq[HiddenToken])

case class LineColumn(line: Int, column: Int)

case class Lines(lines: Array[Line], lastChar: Char) {

  def findLineAndIndex(position:Int):Option[(Line, Int)] = {
    var i = 0

    lines.foreach(l => {
      i = i + 1
      if (position >= l.start && position < l.end) {
        return Some((l, i))
      }
    })

    None
  }

  def toLineColumn(position: Int): Option[LineColumn] =
    findLineAndIndex(position) map {case (line, index) => LineColumn(index, position - line.start)}

  def toFullLineTuple(position: Int): Option[(LineColumn, LineColumn)] =
    findLineAndIndex(position) map {case (line, index) =>  (LineColumn( index, 0 ), LineColumn(index + 1, 0)) }

}

class ScalastyleChecker[T <: FileSpec] {
  def checkFiles(configuration: ScalastyleConfiguration, files: Seq[T]): List[Message[T]] = {
    val checks = configuration.checks.filter(_.enabled)
    StartWork() :: files.flatMap(file => StartFile(file) :: Checker.verifyFile(configuration, checks, file) ::: List(EndFile(file))).toList ::: List(EndWork())
  }

  def checkFilesAsJava(configuration: ScalastyleConfiguration, files: java.util.List[T]): java.util.List[Message[T]] = {
    seqAsJavaList(privateCheckFiles(configuration, collectionAsScalaIterable(files)))
  }

  private[this] def privateCheckFiles(configuration: ScalastyleConfiguration, files: Iterable[T]): Seq[Message[T]] = {
    val checks = configuration.checks.filter(_.enabled)
    StartWork() :: files.flatMap(file => StartFile(file) :: Checker.verifyFile(configuration, checks, file) ::: List(EndFile(file))).toList ::: List(EndWork())
  }

}

case class ScalariformAst(ast: CompilationUnit, comments: List[Comment])

object Checker {
  type CheckerClass = Class[_ <: Checker[_]]

  private def comments(tokens: List[Token]): List[Comment] = tokens.map(t => {
    if (t.associatedWhitespaceAndComments == null) Nil else t.associatedWhitespaceAndComments.comments // scalastyle:ignore null
  }).flatten

  def parseScalariform(source: String): Option[ScalariformAst] = {
    val tokens = ScalaLexer.tokenise(source, true, "2.11.0")

    Some(ScalariformAst(new ScalaParser(tokens.toArray).compilationUnitOrScript(), comments(tokens)))
  }

  def parseLines(source: String): Lines = Lines(source.split("\n").scanLeft(Line("", 0, 0)) {
          case (pl, t) => Line(t, pl.end, pl.end + t.length + 1)
        }.tail, source.charAt(source.length()-1))

  def verifySource[T <: FileSpec](configuration: ScalastyleConfiguration, classes: List[ConfigurationChecker], file: T, source: String): List[Message[T]] = {
    if (source.isEmpty()) {
      Nil
    } else {
      verifySource0(configuration, classes, file, source)
    }
  }

  private def verifySource0[T <: FileSpec](configuration: ScalastyleConfiguration, classes: List[ConfigurationChecker],
                              file: T, source: String): List[Message[T]] = {
    val lines = parseLines(source)
    val scalariformAst = parseScalariform(source)

    val commentFilters = scalariformAst match {
      case Some(ast) if configuration.commentFilter => CommentFilter.findCommentFilters(ast.comments, lines)
      case _ => List[CommentFilter]()
    }

    classes.flatMap(cc => newInstance(cc.className, cc.level, cc.parameters, cc.customMessage, cc.customId)).map(c => c match {
      case c: FileChecker => c.verify(file, c.level, lines, lines)
      case c: ScalariformChecker => scalariformAst match {
        case Some(ast) => c.verify(file, c.level, ast.ast, lines)
        case None => Nil
      }
      case c: CombinedChecker => scalariformAst match {
        case Some(ast) => c.verify(file, c.level, CombinedAst(ast.ast, lines), lines)
        case None => Nil
      }
      case _ => Nil
    }).flatten.filter(m => CommentFilter.filterApplies(m, commentFilters))
  }

  def verifyFile[T <: FileSpec](configuration: ScalastyleConfiguration, classes: List[ConfigurationChecker], file: T): List[Message[T]] = {
    try {
      val s = file match {
        case fs: RealFileSpec => readFile(fs.name, fs.encoding)
        case ss: SourceSpec => ss.contents
      }
      verifySource(configuration, classes, file, s)
    } catch {
      case e: Exception => List(StyleException(file: T, None, message = e.getMessage(), stacktrace = e.getStackTrace().mkString("", "\n", "\n")))
    }
  }

  /**
   * if we pass an encoding in, then we only try that encoding.
   * If there is no encoding passed, we try the default, then UTF-8, then UTF-16, then ISO-8859-1
   */
  def readFile(file: String, encoding: Option[String])(implicit codec: Codec): String = {
    def readFileWithEncoding(file: String, encodings: List[String]): Option[String] = {
      if (encodings.size == 0) {
        None
      } else {
        val encoding = encodings(0)
        try {
          Some(Source.fromFile(file)(encoding).mkString)
        } catch {
          case e: MalformedInputException => {
            // printxxln("caught MalFormedInputException with " + (if (encoding.isDefined) encoding.get else "default (" + codec.charSet + ")") + " encoding")
            readFileWithEncoding(file, encodings.tail)
          }
        }
      }
    }

    val encodings = encoding match {
      case Some(x) => List(x)
      case None => List(codec.charSet.toString(), "UTF-8", "UTF-16", "ISO-8859-1")
    }

    // as far as I can tell, most files should be readable with ISO-8859-1 (though obviously it won't
    // return the correct characters), so I don't know under what circumstances we can get
    // the MalformedInputException (and therefore) RuntimeException here.
    readFileWithEncoding(file, encodings) match {
      case None => throw new RuntimeException("Could not read file, caught MalformedInputException")
      case Some(source) => source
    }
  }

  def newInstance(name: String, level: Level, parameters: Map[String, String], customMessage: Option[String], customId: Option[String]): Option[Checker[_]] = {
    try {
      val clazz = Class.forName(name).asInstanceOf[Class[Checker[_]]]
      val c: Checker[_] = clazz.getConstructor().newInstance().asInstanceOf[Checker[_]]
      c.setParameters(parameters)
      c.setLevel(level)
      c.setCustomMessage(customMessage)
      c.setCustomErrorKey(customId)
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
  protected val errorKey: String;
  var parameters = Map[String, String]();
  var level: Level = WarningLevel;
  var customMessage: Option[String] = None
  var customErrorKey: Option[String] = None

  protected def setParameters(parameters: Map[String, String]) = this.parameters = parameters;
  protected def setLevel(level: Level) = this.level = level;
  protected def setCustomErrorKey(customErrorKey: Option[String]) = this.customErrorKey = customErrorKey
  protected def setCustomMessage(customMessage: Option[String]) = this.customMessage = customMessage
  protected def getInt(parameter: String, defaultValue: Int): Int = Integer.parseInt(parameters.getOrElse(parameter, "" + defaultValue))
  protected def getString(parameter: String, defaultValue: String): String = parameters.getOrElse(parameter, defaultValue)
  protected def getBoolean(parameter: String, defaultValue: Boolean): Boolean = parameters.getOrElse(parameter, "" + defaultValue) == "true"

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

    val sErrorKey = customErrorKey.getOrElse(errorKey)

    p2 match {
      case PositionError(position, args) => StyleError(file, this.getClass(), sErrorKey, level, args, customMessage = customMessage)
      case FileError(args) => StyleError(file, this.getClass(), sErrorKey, level, args, None, None, customMessage)
      case LineError(line, args) => StyleError(file, this.getClass(), sErrorKey, level, args, Some(line), None, customMessage)
      case ColumnError(line, column, args) => StyleError(file, this.getClass(), sErrorKey, level, args, Some(line), Some(column), customMessage)
    }
  }

  def charsBetweenTokens(left: Token, right: Token): Int = right.offset - (left.offset + left.length)

  def verify[T <: FileSpec](file: T, level: Level, ast: A, lines: Lines): List[Message[T]] = {
    verify(ast).map(p => toStyleError(file, p, level, lines))
  }

  def verify(ast: A): List[ScalastyleError]

  protected def isObject(s: String) = (s == "java.lang.Object" || s == "Any")
  protected def isNotObject(s: String) = !isObject(s)
}

trait FileChecker extends Checker[Lines]

trait ScalariformChecker extends Checker[CompilationUnit]

case class CombinedAst(compilationUnit: CompilationUnit, lines: Lines)

trait CombinedChecker extends Checker[CombinedAst]
