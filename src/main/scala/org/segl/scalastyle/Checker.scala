package org.segl.scalastyle

import _root_.scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.ScalaLexer
import _root_.scalariform.parser.ScalaParser
import _root_.scalariform.lexer.Token
import _root_.scalariform.lexer.Tokens._
import scala.io.Source

case class Line(text: String, start: Int, end: Int)

case class Lines(lines: Array[Line]) {
  def translate(position: Int, args: List[String]): ScalastyleError = {
    var i = 0

    lines.foreach(l => {
      i = i + 1
      if (position >= l.start && position < l.end) {
        return ColumnError(i, position - l.start, args)
      }
    })

    FileError()
  }
}

class ScalastyleChecker[T <: FileSpec] {
  def checkFiles(configuration: ScalastyleConfiguration, files: List[T]): List[Message[T]] = {
    StartWork() :: files.flatMap(file => StartFile(file) :: Checker.verifyFile(configuration.checks, file) ::: List(EndFile(file))).toList ::: List(EndWork())
  }
}

object Checker {
  type CheckerClass = Class[_ <: Checker[_]]

  def parseScalariform(source: String): Option[CompilationUnit] = {
    try {
      val (hiddenTokenInfo, tokens) = ScalaLexer.tokeniseFull(source, true)
      Some(new ScalaParser(tokens.toArray).compilationUnitOrScript())
    } catch {
      // TODO improve error logging here
      case e: Exception => None
    }
  }

  private def parseLines(source: String): Lines = Lines(source.split("\n").scanLeft(Line("", 0, 0)) {
          case (pl, t) => Line(t, pl.end, pl.end + t.length + 1)
        }.tail)

  def verifySource[T <: FileSpec](classes: List[ConfigCheck], file: T, source: String): List[Message[T]] = {
    lazy val lines = parseLines(source)
    lazy val scalariformAst = parseScalariform(source)

    classes.flatMap(cc => newInstance(cc.className, cc.level, cc.parameters)).map(c => c match {
      case c: FileChecker => c.verify(file, c.level, lines, lines)
      case c: ScalariformChecker => scalariformAst match {
        case Some(x) => c.verify(file, c.level, scalariformAst.get, lines)
        case None => List[Message[T]]()
      }
      case _ => List[Message[T]]()
    }).flatten
  }

  def verifyFile[T <: FileSpec](classes: List[ConfigCheck], file: T): List[Message[T]] = verifySource(classes, file, Source.fromFile(file.name).mkString)

  def newInstance(name: String, level: Level, parameters: Map[String, String]): Option[Checker[_]] = {
    try {
      val clazz = Class.forName(name).asInstanceOf[Class[Checker[_]]]
      val c: Checker[_] = clazz.getConstructor().newInstance().asInstanceOf[Checker[_]]
      c.setParameters(parameters)
      c.setLevel(level)
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

  def setParameters(parameters: Map[String, String]) = this.parameters = parameters;
  def setLevel(level: Level) = this.level = level;
  def getInt(parameter: String, defaultValue: Int) = Integer.parseInt(parameters.getOrElse(parameter, "" + defaultValue))
  def getString(parameter: String, defaultValue: String) = parameters.getOrElse(parameter, defaultValue)

  protected def toStyleError[T <: FileSpec](file: T, p: ScalastyleError, level: Level, lines: Lines): Message[T] = {
    val p2 = p match {
      case PositionError(position, args) => lines.translate(position, args)
      case _ => p
    }

    p2 match {
      case PositionError(position, args) => StyleError(file, this.getClass(), errorKey, level, args)
      case FileError(args) => StyleError(file, this.getClass(), errorKey, level, args, None, None)
      case LineError(line, args) => StyleError(file, this.getClass(), errorKey, level, args, Some(line), None)
      case ColumnError(line, column, args) => StyleError(file, this.getClass(), errorKey, level, args, Some(line), Some(column))
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
