package org.segl.scalastyle

trait Output[T <: FileSpec] {
  def output(messages: List[Message[T]])
}

class TextOutput[T <: FileSpec] extends Output[T] {
  private val messageHelper = new MessageHelper()
  override def output(messages: List[Message[T]]) = messages.foreach(message)

  private def message(m: Message[T]) = m match {
    case StartWork() => println("Starting scalastyle")
    case EndWork() => println("Scalastyle done. Now go and fix your code.")
    case StartFile(file) => println("start file " + file)
    case EndFile(file) => println("end file " + file)
    case StyleError(file, clazz, key, args, line, column) => {
      println("error" + print("file", file.name) + print("message", messageHelper.getMessage(clazz, key, args)) + print("line", line) + print("column", column))
    }
    case StyleException(file, clazz, message, stacktrace, line, column) => {
      println("error" + print("file", file.name) + print("message", message) + print("line", line) + print("column", column))
    }
  }

  private def print(s: String, no: Option[Int]): String = if (no.isDefined) print(s, "" + no.get) else ""
  private def print(s: String, value: String): String = " " + s + "=" + value
}
