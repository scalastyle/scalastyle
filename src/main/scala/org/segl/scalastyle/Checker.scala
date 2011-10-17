package org.segl.scalastyle

case class AST(lines: List[String])

import java.lang.reflect.Constructor

object ScalastyleChecker {
  val checkers: List[Class[_ <: Checker]] = List(classOf[FileTabChecker], classOf[FileLineLengthChecker], classOf[FileLengthChecker], classOf[DivisionByZeroChecker])
}

class ScalastyleChecker {
  lazy val foo = 5 / 0;
  
//  def checkFiles(files: List[String]) = StartWork() :: files.par.flatMap(file => checkFile(file, parse(file))).toList ::: List(EndWork()) 

//  def parse(file: String): AST = AST(scala.io.Source.fromFile(file).getLines.toList)

//  def checkFile(file: String, ast: AST) = StartFile(file) :: ScalastyleChecker.checkers.flatMap(_.verify(file, ast).toList) ::: List(EndFile(file)) 
}