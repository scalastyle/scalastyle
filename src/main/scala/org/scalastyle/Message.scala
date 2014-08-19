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

import java.text.MessageFormat
import com.typesafe.config.Config
import java.io.FileWriter

trait FileSpec {
  def name: String
}

class RealFileSpec(val name: String, val encoding: Option[String]) extends FileSpec

class SourceSpec(val name: String, val contents: String) extends FileSpec

// for the methods in this class, if a key does not exist
// we return the key so something always appears
class MessageHelper(config: Config) {

  def text(key: String): String = getMessage(key + ".text", List())

  def label(key: String): String = getMessage(key + ".label", List())
  def description(key: String): String = getMessage(key + ".description", List())

  private[this] def getMessage(key: String, args: List[String]) = {
    try {
      val pattern = config.getString(key)
      MessageFormat.format(pattern, args.map(_.asInstanceOf[AnyRef]): _*)
    } catch {
      // If there is no message, just use the key
      case _: Throwable => MessageFormat.format(key, args.map(_.asInstanceOf[AnyRef]): _*)
    }
  }

  def message(key: String, args: List[String]): String = {
    // Use ClassLoader of the class from which the message came
    getMessage(key + ".message", args)
  }
}

sealed abstract class Message[+T <: FileSpec]()

case class StartWork[+T <: FileSpec]() extends Message[T]
case class EndWork[+T <: FileSpec]() extends Message[T]

case class StartFile[+T <: FileSpec](fileSpec: T) extends Message[T]
case class EndFile[+T <: FileSpec](fileSpec: T) extends Message[T]

case class StyleError[+T <: FileSpec](fileSpec: T, clazz: Class[_ <: Checker[_]], key: String,
                                      level: Level, args: List[String], lineNumber: Option[Int] = None,
                                      column: Option[Int] = None, customMessage: Option[String] = None) extends Message[T] {
  override def toString(): String = "StyleError key=" + key + " args=" + args + " lineNumber=" + lineNumber +
                                          " column=" + column + " customMessage=" + customMessage
}
case class StyleException[+T <: FileSpec](fileSpec: T, clazz: Option[Class[_ <: Checker[_]]], message: String,
                                          stacktrace: String, lineNumber: Option[Int] = None, column: Option[Int] = None) extends Message[T]

sealed abstract class ScalastyleError
case class PositionError(position: Int, args: List[String] = List[String]()) extends ScalastyleError
case class FileError(args: List[String] = List[String]()) extends ScalastyleError
case class LineError(line: Int, args: List[String] = List[String]()) extends ScalastyleError
case class ColumnError(line: Int, column: Int, args: List[String] = List[String]()) extends ScalastyleError

