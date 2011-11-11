package org.segl.scalastyle

import _root_.scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.ScalaLexer
import _root_.scalariform.parser.ScalaParser
import _root_.scalariform.lexer.Token
import _root_.scalariform.lexer.Tokens._
import scala.io.Source

case class Lines(lines: Array[String])

class ScalastyleChecker {
  def checkFiles(configuration: ScalastyleConfiguration, files: List[String]): List[Message] = {
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
  
  def verifySource(classes: List[ConfigCheck], file: String, source: String): List[Message] = {
    lazy val lines = parseLines(source)
    lazy val scalariformAst = parseScalariform(source)

    classes.map(cc => newInstance(getClass(cc.className), cc.parameters)).flatMap(c => c match {
      case c: FileChecker => c.verify(file, lines)
      case c: ScalariformChecker => c.verify(file, scalariformAst)
      case _ => List[Message]()
    })
  }
  
  def getClass(name: String) = Class.forName(name).asInstanceOf[Class[Checker]]

  def verifyFile(classes: List[ConfigCheck], file: String): List[Message] = verifySource(classes, file, Source.fromFile(file).mkString)
  def newInstance(clazz: Class[Checker], parameters: Map[String, String]) = {
    val c: Checker = clazz.getConstructor().newInstance().asInstanceOf[Checker]
    c.setParameters(parameters)
    c
  }
}

trait Checker {
  var parameters = Map[String, String]();
  
  def setParameters(parameters: Map[String, String]) = this.parameters = parameters;
  def getInt(parameter: String, defaultValue: Int) = Integer.parseInt(parameters.getOrElse(parameter, "" + defaultValue))
}

trait FileChecker extends Checker {
  def verify(file: String, lines: Lines): List[Message]
}

trait ScalariformChecker extends Checker {
  def verify(file: String, ast: CompilationUnit): List[Message]
}


