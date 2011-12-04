package org.segl.scalastyle

trait FileSpec {
  def name: String
  def file: java.io.File
}

sealed abstract class Message[+T <: FileSpec]()

case class StartWork[+T <: FileSpec]() extends Message[T]
case class EndWork[+T <: FileSpec]() extends Message[T]

case class StartFile[+T <: FileSpec](fileSpec: T) extends Message[T]
case class EndFile[+T <: FileSpec](fileSpec: T) extends Message[T]

case class StyleError[+T <: FileSpec](fileSpec: T, key: String, lineNumber: Option[Int] = None, column: Option[Int] = None, position: Option[Int] = None) extends Message[T]
case class StyleException[+T <: FileSpec](fileSpec: T, message: String, stacktrace: String, lineNumber: Option[Int] = None, column: Option[Int] = None) extends Message[T]

case class Position(lineNumber: Option[Int] = None, column: Option[Int] = None, position: Option[Int] = None)
