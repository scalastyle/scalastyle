package org.segl.scalastyle

import _root_.scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.ScalaLexer
import _root_.scalariform.parser.ScalaParser
import _root_.scalariform.lexer.Token
import _root_.scalariform.lexer.Tokens._
import scala.io.Source

case class Line(text: String, start: Int, end: Int)

case class Lines(lines: Array[Line]) {
  def translate(position: Int): ScalastyleError = {
    var i = 0

    lines.foreach(l => {
      i = i + 1
      if (position >= l.start && position < l.end) {
        return ColumnError(i, position - l.start)
      }
    })

    FileError()
  }
}

class ScalastyleChecker[T <: FileSpec] {
  def checkFiles(configuration: ScalastyleConfiguration, files: List[T]): List[Message[T]] = {
    StartWork() :: files.flatMap(file => StartFile(file) :: Checker.verifyFile(configuration.checks, file) ::: List(EndFile(file)) ).toList ::: List(EndWork())
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

  private def parseLines(source: String): Lines = Lines(source.split("\n").scanLeft(Line("", 0, 0)){ case (pl, t) => Line(t, pl.end, pl.end + t.length + 1) }.tail)

  def verifySource[T <: FileSpec](classes: List[ConfigCheck], file: T, source: String): List[Message[T]] = {
    lazy val lines = parseLines(source)
    lazy val scalariformAst = parseScalariform(source)

    classes.map(cc => newInstance(getClass(cc.className), cc.parameters)).flatMap(c => c match {
      case c: FileChecker => c.verify(file, lines, lines)
      case c: ScalariformChecker => scalariformAst match {
        case Some(x) => c.verify(file, scalariformAst.get, lines)
        case None => List[Message[T]]()
      }
      case _ => List[Message[T]]()
    })
  }

  def getClass(name: String) = Class.forName(name).asInstanceOf[Class[Checker[_]]]

  def verifyFile[T <: FileSpec](classes: List[ConfigCheck], file: T): List[Message[T]] = verifySource(classes, file, Source.fromFile(file.name).mkString)

  def newInstance(clazz: Class[Checker[_]], parameters: Map[String, String]) = {
    val c: Checker[_] = clazz.getConstructor().newInstance().asInstanceOf[Checker[_]]
    c.setParameters(parameters)
    c
  }
}

trait Checker[A] {
  val errorKey: String;
  var parameters = Map[String, String]();

  def setParameters(parameters: Map[String, String]) = this.parameters = parameters;
  def getInt(parameter: String, defaultValue: Int) = Integer.parseInt(parameters.getOrElse(parameter, "" + defaultValue))
  def getString(parameter: String, defaultValue: String) = parameters.getOrElse(parameter, defaultValue)

  protected def toStyleError[T <: FileSpec](file: T, p: ScalastyleError, lines: Lines): Message[T] = {
    val p2 = p match {
      case PositionError(position) => lines.translate(position)
      case _ => p
    }

    p2 match {
      case PositionError(position) => StyleError(file, errorKey)
      case FileError() => StyleError(file, errorKey, None, None)
      case LineError(line) => StyleError(file, errorKey, Some(line), None)
      case ColumnError(line, column) => StyleError(file, errorKey, Some(line), Some(column))
    }
  }

  def charsBetweenTokens(left: Token, right: Token): Int = right.startIndex - (left.startIndex + left.length)

  def verify[T <: FileSpec](file: T, ast: A, lines: Lines): List[Message[T]] = {
    verify(ast).map(p => toStyleError(file, p, lines))
  }

  def verify(ast: A): List[ScalastyleError]
}

trait FileChecker extends Checker[Lines]

trait ScalariformChecker extends Checker[CompilationUnit]
