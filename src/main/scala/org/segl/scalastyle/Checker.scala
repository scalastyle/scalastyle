package org.segl.scalastyle

case class AST(lines: List[String])

class ScalastyleChecker {
  val checkers: List[_ <: Checker] = List(new FileTabChecker(), new FileLineLengthChecker(), new FileLengthChecker())

  def checkFiles(files: List[String]) = StartWork() :: files.par.flatMap(file => checkFile(file, parse(file))).toList ::: List(EndWork()) 

  def parse(file: String): AST = AST(scala.io.Source.fromFile(file).getLines.toList)

  def checkFile(file: String, ast: AST) = StartFile(file) :: checkers.flatMap(_.verify(file, ast).toList) ::: List(EndFile(file)) 
}