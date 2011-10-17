package org.segl.scalastyle

import scala.tools.nsc
import nsc.Global
import nsc.plugins.Plugin
import nsc.Phase
import nsc.plugins.PluginComponent

abstract class Checker(global: Global) {
  import global._
  
  def verify(file: String, ast: AST): List[Message] = List()
  def verify(file: String, ast: Global#Tree): List[Message] = List()
}

abstract class FileChecker(global: Global) extends Checker(global) {
  def verify(file: String, ast: AST): List[Message]
  override def verify(file: String, ast: Global#Tree): List[Message] = List()
}

abstract class PluginChecker(global: Global) extends Checker(global) {
  override def verify(file: String, ast: AST): List[Message] = List()
  def verify(file: String, ast: Global#Tree): List[Message]
}

class FileTabChecker(global: Global) extends FileChecker(global) {
  override def verify(file: String, ast: AST): List[Message] = {
    for (line <- ast.lines.zipWithIndex;
    		if line._1.contains('\t')) yield {
      StyleError(file, "line.contains.tab", Some(line._2 + 1), Some(line._1.indexOf('\t')))
    }
  }
}

class FileLineLengthChecker(global: Global) extends FileChecker(global) {
  override def verify(file: String, ast: AST): List[Message] = {
    for (line <- ast.lines.zipWithIndex;
    		if line._1.length() > 80) yield {
      StyleError(file, "line.size.limit", Some(line._2 + 1))
    }
  }
}

class FileLengthChecker(global: Global) extends FileChecker(global) {
  override def verify(file: String, ast: AST): List[Message] = {
    if (ast.lines.size > 10) List(StyleError(file, "file.size.limit")) else List()
  }
}


class DivisionByZeroChecker(implicit global: Global) extends PluginChecker(global: Global) {
  import global._

  override def verify(file: String, tree: Global#Tree): List[Message] = {
    tree match {
      case Apply(Select(_, nme.DIV), List(Literal(Constant(0)))) => List(StyleError(file, "div.by.zero", Some(tree.pos.line+1), Some(tree.pos.column)))
      case Apply(Select(_, nme.MOD), List(Literal(Constant(0)))) => List(StyleError(file, "div.by.zero", Some(tree.pos.line+1), Some(tree.pos.column)))
      case _ => List()
    }
  }
}
