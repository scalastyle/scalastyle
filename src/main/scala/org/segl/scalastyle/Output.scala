package org.segl.scalastyle

trait Output {
  def output(messages: List[Message])
}

class TextOutput extends Output {
  override def output(messages: List[Message]) = messages.foreach(message)

  private def message(m: Message) = m match {
    case StartWork() => println("Starting scalastyle")
    case EndWork() => println("Scalastyle done. Now go and fix your code.")
    case StartFile(file) => println("start file " + file)
    case EndFile(file) => println("end file " + file)
    case StyleError(file, key, line, column, position) => println("error" + print("file", file) + print("key", key) + printOption("line", line) + printOption("column", column) + printOption("position", position))
    case StyleException(file, message, stacktrace, line, column) => println("error" + print("file", file) + print("message", message) + printOption("line", line) + printOption("column", column))
  }

  private def printOption(s: String, no: Option[Int]) = if (no.isDefined) print(s, "" + no.get) else "" 
  private def print(s: String, value: String) = " " + s + "=" + value 
}

//class XmlOutput extends Output {
//  override def message(m: Message) = m match {
//    case StartWork() => println("Starting scalastyle")
//    case EndWork() => println("Scalastyle done. Now go and fix your code.")
//  }
//  
//  override def flush = {
//
//  }
//}