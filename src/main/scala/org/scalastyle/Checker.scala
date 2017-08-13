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

import java.nio.charset.MalformedInputException

import _root_.scalariform.lexer.Comment
import _root_.scalariform.lexer.HiddenToken
import _root_.scalariform.lexer.ScalaLexer
import _root_.scalariform.lexer.Token
import _root_.scalariform.parser.CompilationUnit
import _root_.scalariform.parser.ScalaParser
import scala.annotation.tailrec
import scala.collection.JavaConversions.collectionAsScalaIterable
import scala.collection.JavaConversions.seqAsJavaList
import scala.io.Codec
import scala.io.Source

case class Line(text: String, start: Int, end: Int)

case class HiddenTokenInfo(tokens: Seq[HiddenToken])

case class LineColumn(line: Int, column: Int)

case class Lines(lines: Array[Line], lastChar: Char) {

  def findLineAndIndex(position:Int): Option[(Line, Int)] = {
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

class ScalastyleChecker[T <: FileSpec](classLoader: Option[ClassLoader] = None) {
  def checkFiles(configuration: ScalastyleConfiguration, files: Seq[T]): List[Message[T]] = {
    privateCheckFiles(configuration, files).toList
  }

  def checkFilesAsJava(configuration: ScalastyleConfiguration, files: java.util.List[T]): java.util.List[Message[T]] = {
    seqAsJavaList(privateCheckFiles(configuration, collectionAsScalaIterable(files)))
  }

  private[this] def privateCheckFiles(configuration: ScalastyleConfiguration, files: Iterable[T]): Seq[Message[T]] = {
    val checks = configuration.checks.filter(_.enabled)
    val checkerUtils = new CheckerUtils(classLoader)
    StartWork() :: files.flatMap(file => StartFile(file) :: checkerUtils.verifyFile(configuration, checks, file) :::
        List(EndFile(file))).toList ::: List(EndWork())
  }

}

case class ScalariformAst(ast: CompilationUnit, comments: List[Comment])

object Checker {
  def parseLines(source: String): Lines = {

    // Split lines by supported EOL sequences. Windows text files use a carriage return ("\r") immediately followed by a
    // linefeed ("\n"), while Unix/Linux/BSD/etc. text files use just a linefeed. Other EOL sequences are currently
    // unsupported. Note that split removes the matching regular expression, so that the array of lines excludes EOL
    // sequences.
    Lines(source.split("\n").scanLeft(Line("", 0, 0)) {
      case (pl, t) =>
        val text = if (t.endsWith("\r")) t.init else t
        Line(text, pl.end, pl.end + t.length + 1)
    }.tail, source.charAt(source.length()-1))
  }
}

class CheckerUtils(classLoader: Option[ClassLoader] = None) {
  private def comments(tokens: List[Token]): List[Comment] = tokens.map(t => {
    if (t.associatedWhitespaceAndComments == null) Nil else t.associatedWhitespaceAndComments.comments // scalastyle:ignore null
  }).flatten

  def parseScalariform(source: String): Option[ScalariformAst] = {
    val tokens = ScalaLexer.tokenise(source, true, "2.11.0")

    Some(ScalariformAst(new ScalaParser(tokens.toArray).compilationUnitOrScript(), comments(tokens)))
  }

  def verifySource[T <: FileSpec](configuration: ScalastyleConfiguration, classes: List[ConfigurationChecker], file: T, source: String): List[Message[T]] = {
    if (source.isEmpty()) {
      Nil
    } else {
      verifySource0(configuration, classes, file, source)
    }
  }

  private def verifySource0[T <: FileSpec](configuration: ScalastyleConfiguration, classes: List[ConfigurationChecker],
                              file: T, source: String): List[Message[T]] = {
    val lines = Checker.parseLines(source)
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
    @tailrec
    def readFileWithEncoding(file: String, encodings: List[String]): Option[String] = {
      if (encodings.isEmpty) {
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

  private def newInstance(name: String, level: Level, parameters: Map[String, String], customMessage: Option[String],
      customId: Option[String]): Option[Checker[_]] = {
    try {
      val cl: ClassLoader = classLoader.getOrElse(this.getClass().getClassLoader())

      val clazz = Class.forName(name, true, cl)
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
  protected val errorKey: String
  var parameters = Map[String, String]()
  var level: Level = WarningLevel
  var customMessage: Option[String] = None
  var customErrorKey: Option[String] = None

  def setParameters(parameters: Map[String, String]): Unit = this.parameters = parameters
  def setLevel(level: Level): Unit = this.level = level
  def setCustomErrorKey(customErrorKey: Option[String]): Unit = this.customErrorKey = customErrorKey
  def setCustomMessage(customMessage: Option[String]): Unit = this.customMessage = customMessage
  protected def getInt(parameter: String, defaultValue: Int): Int = Integer.parseInt(parameters.getOrElse(parameter, "" + defaultValue))
  protected def getString(parameter: String, defaultValue: String): String = parameters.getOrElse(parameter, defaultValue)
  protected def getBoolean(parameter: String, defaultValue: Boolean): Boolean = parameters.getOrElse(parameter, "" + defaultValue) == "true"

  protected def toStyleError[T <: FileSpec](file: T, p: ScalastyleError, level: Level, lines: Lines): Message[T] = {
    val p2 = p match {
      case PositionError(position, args, errorKey) => {
        lines.toLineColumn(position) match {
          case Some(LineColumn(line, column)) => ColumnError(line, column, args, errorKey)
          case None => FileError
        }
      }
      case _ => p
    }

    val sErrorKey = customErrorKey.getOrElse(errorKey)

    p2 match {
      case PositionError(position, args, errorKey) => StyleError(file, this.getClass(), errorKey.getOrElse(sErrorKey), level, args, customMessage = customMessage)
      case FileError(args, errorKey) => StyleError(file, this.getClass(), errorKey.getOrElse(sErrorKey), level, args, None, None, customMessage)
      case LineError(line, args, errorKey) => StyleError(file, this.getClass(), errorKey.getOrElse(sErrorKey), level, args, Some(line), None, customMessage)
      case ColumnError(line, column, args, errorKey) => StyleError(file, this.getClass(), errorKey.getOrElse(sErrorKey), level, args, Some(line), Some(column), customMessage)
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
