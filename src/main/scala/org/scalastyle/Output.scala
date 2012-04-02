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

trait Output[T <: FileSpec] {
  def output(messages: List[Message[T]]): OutputResult
}

case class OutputResult(errors: Int, warnings: Int)

class TextOutput[T <: FileSpec](verbose: Boolean = false, quiet: Boolean = false) extends Output[T] {
  private val messageHelper = new MessageHelper(getClass().getClassLoader())
  override def output(messages: List[Message[T]]): OutputResult = {
    messages.foreach(message)
    OutputResult(errors, warnings)
  }
  private var errors = 0
  private var warnings = 0

  private def message(m: Message[T]) = m match {
    case StartWork() => if (verbose) println("Starting scalastyle")
    case EndWork() => {
      if (!quiet) println("Found " + errors + " errors")
      if (!quiet) println("Found " + warnings + " warnings")
    }
    case StartFile(file) => if (verbose) println("start file " + file)
    case EndFile(file) => if (verbose) println("end file " + file)
    case StyleError(file, clazz, key, level, args, line, column) => {
      println(messageHelper.text(level.name) + print("file", file.name) +
          print("message", messageHelper.message(clazz.getClassLoader(), key, args)) +
          print("line", line) + print("column", column))
      level match {
        case WarningLevel => warnings += 1
        case _ => errors += 1
      }
    }
    case StyleException(file, clazz, message, stacktrace, line, column) => {
      println("error" + print("file", file.name) + print("message", message) + print("line", line) + print("column", column))
    }
  }

  private def print(s: String, no: Option[Int]): String = if (no.isDefined) print(s, "" + no.get) else ""
  private def print(s: String, value: String): String = " " + s + "=" + value
}