package org.segl.scalastyle

case class AST(lines: List[String])

case class Message(file: String, key: String, lineNumber: Int, column: Int)

case class Error(filename: String, message: Message)

trait Listener {
  def start(): Unit = {}
  def end(): Unit = {}

  def fileStart(file: String): Unit = {}
  def fileEnd(file: String): Unit = {}

  def error(file: String, key: String, lineNumber: Int, column: Int) = {}
}

class ScalastyleChecker {
  val checkers: List[_ <: Checker] = List(new FileTabChecker(), new FileLineLengthChecker())

  def checkFiles(listener: Listener, files: Array[String]): Array[Message] = {
    listener.start
    val result = files.flatMap(file => checkFile(listener, file, parse(file)))
    listener.end

    result
  }

  def parse(file: String): AST = AST(scala.io.Source.fromFile(file).getLines.toList)

  def checkFile(listener: Listener, file: String, ast: AST): List[Message] = {
    listener.fileStart(file)
    val result = checkers.flatMap(_.verify(file, ast).toList)
    listener.fileEnd(file)

    result
  }

}