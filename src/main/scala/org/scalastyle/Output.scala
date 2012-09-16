// Copyright (C) 2011-2012 the original author or authors.
// See the LICENCE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package org.scalastyle

import scala.xml.XML
import scala.xml.Elem

object Output {
  // messageHelper passed in here to work around a scala compiler bug?
  def findMessage(messageHelper: MessageHelper, clazz: Class[_], key: String, args: List[String], customMessage: Option[String]): String = {
    customMessage match {
      case Some(s) => s
      case None => messageHelper.message(clazz.getClassLoader(), key, args)
    }
  }
}

trait Output[T <: FileSpec] {
  private var errors = 0
  private var warnings = 0
  private var files = 0

  def output(messages: Seq[Message[T]]): OutputResult = {
    messages.foreach(m => { eachMessage(m); message(m) })
    OutputResult(files, errors, warnings)
  }

  def eachMessage(m: Message[T]): Unit = m match {
    case StartWork() =>
    case EndWork() =>
    case StartFile(file) => files += 1
    case EndFile(file) =>
    case StyleError(file, clazz, key, level, args, line, column, customMessage) => level match {
      case WarningLevel => warnings += 1
      case _ => errors += 1
    }
    case StyleException(file, clazz, message, stacktrace, line, column) => errors += 1
  }
  def message(m: Message[T]): Unit
}

case class OutputResult(files: Int, errors: Int, warnings: Int)

class TextOutput[T <: FileSpec](verbose: Boolean = false, quiet: Boolean = false) extends Output[T] {
  private val messageHelper = new MessageHelper(this.getClass().getClassLoader())

  // scalastyle:off regex
  override def message(m: Message[T]): Unit = m match {
    case StartWork() => if (verbose) println("Starting scalastyle")
    case EndWork() =>
    case StartFile(file) => if (verbose) println("start file " + file)
    case EndFile(file) => if (verbose) println("end file " + file)
    case StyleError(file, clazz, key, level, args, line, column, customMessage) => {
      println(messageHelper.text(level.name) + print("file", file.name) +
          print("message", Output.findMessage(messageHelper, clazz, key, args, customMessage)) +
          print("line", line) + print("column", column))
    }
    case StyleException(file, clazz, message, stacktrace, line, column) => {
      println("error" + print("file", file.name) + print("message", message) + print("line", line) + print("column", column))
    }
  }
  // scalastyle:on regex

  private def print(s: String, no: Option[Int]): String = if (no.isDefined) print(s, "" + no.get) else ""
  private def print(s: String, value: String): String = " " + s + "=" + value
}

object XmlOutput {
  def save[T <: FileSpec](target: String, messages: Seq[Message[T]]) {
    val width = 1000;
    val step = 1;
    val messageHelper = new MessageHelper(this.getClass().getClassLoader())

    val s = new XmlPrettyPrinter(width, step).format(toCheckstyleFormat(messageHelper, messages))
    // scalastyle:off regex
    printToFile(new java.io.File(target)){ _.println(s) }
    // scalastyle:on regex
  }

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }
  case class Alert(filename: String, severity: String, message: String, source: Option[Class[_]], line: Option[Int], column: Option[Int])

  private[this] def toCheckstyleFormat[T <: FileSpec](messageHelper: MessageHelper, messages: Seq[Message[T]]): Elem = {
    <checkstyle version="5.0">{
      messages.collect {
        case StyleError(file, clazz, key, level, args, line, column, customMessage) =>
          Alert(file.name, messageHelper.text(level.name), Output.findMessage(messageHelper, clazz, key, args, customMessage), Some(clazz), line, column)
        case StyleException(file, clazz, message, stacktrace, line, column) =>
          Alert(file.name, "error", message, clazz, line, column)
      }.groupBy{_.filename}.map { case (filename, alerts) =>
        <file name={filename}>{
          alerts.map {
            case Alert(filename, severity, message, source, line, column) => {
              val s = source.collect { case x: Class[_] => x.getName() }
              <error severity={severity} message={message}/> % attr("source", s) % attr("line", line) % attr("column", column)
            }
          }
        }</file>
      }
    }</checkstyle>
  }

  private[this] def attr(name: String, value: Option[Any]): xml.MetaData = value match {
    case Some(x) => xml.Attribute("", name, x.toString, xml.Null)
    case None => xml.Null
  }
}
