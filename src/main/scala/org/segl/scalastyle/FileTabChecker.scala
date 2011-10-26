package org.segl.scalastyle

import java.lang.reflect.Constructor;

import _root_.scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.ScalaLexer
import _root_.scalariform.parser.ScalaParser
import _root_.scalariform.lexer.Token
import _root_.scalariform.lexer.Tokens._
import scala.io.Source

case class Lines(lines: Array[String])

object Checker {
  type CheckerClass = Class[_ <: Checker]

  def parseScalariform(source: String) = {
    val (hiddenTokenInfo, tokens) = ScalaLexer.tokeniseFull(source, true)
    new ScalaParser(tokens.toArray).compilationUnitOrScript()
  }

  private def parseLines(source: String): Lines = Lines(source.split("\n"));

  def verifySource(classes: List[CheckerClass], file: String, source: String): List[Message] = {
    lazy val lines = parseLines(source)
    lazy val scalariformAst = parseScalariform(source)

    classes.map(clazz => newInstance(clazz)).flatMap(c => c match {
      case c: LinesChecker => c.verify(file, lines)
      case c: ScalariformChecker => c.verify(file, scalariformAst)
      case _ => List[Message]()
    })
  }
  
  def verifyFile(classes: List[CheckerClass], file: String): List[Message] = verifySource(classes, file, Source.fromFile(file).mkString)
  def newInstance[T](clazz: Class[T]) = clazz.getConstructor().newInstance().asInstanceOf[T]
}

trait Checker
trait LinesChecker extends Checker {
  def verify(file: String, lines: Lines): List[Message]
}

trait ScalariformChecker extends Checker {
  def verify(file: String, ast: CompilationUnit): List[Message]
}

class FileTabChecker extends LinesChecker {
  def verify(file: String, lines: Lines): List[Message] = {
    val errors = for (
      line <- lines.lines.zipWithIndex;
      if line._1.contains('\t')
    ) yield {
      StyleError(file, "line.contains.tab", Some(line._2 + 1), Some(line._1.indexOf('\t')))
    }
    
    return errors.toList
  }
}

class FileLineLengthChecker extends LinesChecker {
  def verify(file: String, lines: Lines): List[Message] = {
    val errors = for (
      line <- lines.lines.zipWithIndex;
      if line._1.length() > 80
    ) yield {
      StyleError(file, "line.size.limit", Some(line._2 + 1))
    }
    
    return errors.toList
  }
}

class FileLengthChecker extends LinesChecker {
  override def verify(file: String, ast: Lines): List[Message] = {
    if (ast.lines.size > 10) List(StyleError(file, "file.size.limit")) else List()
  }
}

class SpacesAfterPlusChecker extends ScalariformChecker {
  def verify(file: String, ast: CompilationUnit): List[Message] = {
    val it = for (
      List(left, right) <- ast.tokens.sliding(2);
      if (left.tokenType == PLUS && left.startIndex + 1 == right.startIndex)
    ) yield {
      StyleError(file, "spaces.after.plus", position = Some(left.startIndex))
    }

    return it.toList
  }
}