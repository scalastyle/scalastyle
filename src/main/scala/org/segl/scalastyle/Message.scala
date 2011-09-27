package org.segl.scalastyle

sealed abstract class Message

case class StartWork extends Message
case class EndWork extends Message

case class StartFile(name: String) extends Message
case class EndFile(name: String) extends Message

case class StyleError(file: String, key: String, lineNumber: Option[Int] = None, column: Option[Int] = None) extends Message
case class StyleException(file: String, message: String, stacktrace: String, lineNumber: Option[Int] = None, column: Option[Int] = None) extends Message

