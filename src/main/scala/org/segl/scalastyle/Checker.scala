package org.segl.scalastyle

import _root_.scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.ScalaLexer
import _root_.scalariform.parser.ScalaParser
import _root_.scalariform.lexer.Token
import _root_.scalariform.lexer.Tokens._
import scala.io.Source

case class Lines(lines: Array[String])

class ScalastyleChecker[T <: FileSpec] {
  def checkFiles(configuration: ScalastyleConfiguration, files: List[T]): List[Message[T]] = {
    StartWork() :: files.flatMap(file => Checker.verifyFile(configuration.checks, file)).toList ::: List(EndWork())
  }
}

object Checker {
  type CheckerClass = Class[_ <: Checker]

  def parseScalariform(source: String) = {
    val (hiddenTokenInfo, tokens) = ScalaLexer.tokeniseFull(source, true)
    new ScalaParser(tokens.toArray).compilationUnitOrScript()
  }

  private def parseLines(source: String): Lines = Lines(source.split("\n"));

  def verifySource[T <: FileSpec](classes: List[ConfigCheck], file: T, source: String): List[Message[T]] = {
    lazy val lines = parseLines(source)
    lazy val scalariformAst = parseScalariform(source)

    classes.map(cc => newInstance(getClass(cc.className), cc.parameters)).flatMap(c => c match {
      case c: FileChecker => c.verify(file, lines)
      case c: ScalariformChecker => c.verify(file, scalariformAst)
      case _ => List[Message[T]]()
    })
  }

  def getClass(name: String) = Class.forName(name).asInstanceOf[Class[Checker]]

  def verifyFile[T <: FileSpec](classes: List[ConfigCheck], file: T): List[Message[T]] = verifySource(classes, file, Source.fromFile(file.name).mkString)

  def newInstance(clazz: Class[Checker], parameters: Map[String, String]) = {
    val c: Checker = clazz.getConstructor().newInstance().asInstanceOf[Checker]
    c.setParameters(parameters)
    c
  }
}

trait Checker {
  val errorKey: String;
  var parameters = Map[String, String]();

  def setParameters(parameters: Map[String, String]) = this.parameters = parameters;
  def getInt(parameter: String, defaultValue: Int) = Integer.parseInt(parameters.getOrElse(parameter, "" + defaultValue))
  def getString(parameter: String, defaultValue: String) = parameters.getOrElse(parameter, defaultValue)
}

trait FileChecker extends Checker {
  def verify[T <: FileSpec](file: T, lines: Lines): List[Message[T]] = {
    verify(lines).map(p => new StyleError(file, errorKey, p.lineNumber, p.column, p.position))
  }

  def verify(lines: Lines): List[Position]
}

trait ScalariformChecker extends Checker {
  def verify(ast: CompilationUnit): List[Position]
  
  final def verify[T <: FileSpec](file: T, ast: CompilationUnit): List[Message[T]] = {
    verify(ast).map(p => new StyleError(file, errorKey, p.lineNumber, p.column, p.position))
  }

  def charsBetweenTokens(left: Token, right: Token): Int = right.startIndex - (left.startIndex + left.length)
}
