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
import java.util.ResourceBundle
import java.util.Locale
import scala.collection.mutable.HashMap

trait FileSpec {
  def name: String
}

// for the methods in this class, if a key does not exist
// we return the key so something always appears
class MessageHelper(classLoader: ClassLoader) {
  val bundles = HashMap[String, ResourceBundle]()

  def text(key: String) = getMessage(classLoader, key + ".text", List())

  def label(key: String) = getMessage(classLoader, key + ".label", List())
  def description(key: String) = getMessage(classLoader, key + ".description", List())

  private[this] def getMessage(classLoader: ClassLoader, key: String, args: List[String]) = {
    try {
      val bundle = ResourceBundle.getBundle("scalastyle_messages", Locale.getDefault(), classLoader)

      // Use ClassLoader of the class from which the message came
      val pattern = bundle.getString(key)
      MessageFormat.format(pattern, args.map(_.asInstanceOf[AnyRef]): _*)
    } catch {
      // If there is no message, just use the key
      case _ => MessageFormat.format(key, args.map(_.asInstanceOf[AnyRef]): _*)
    }
  }

  def message(classLoader: ClassLoader, key: String, args: List[String]) = {
    // Use ClassLoader of the class from which the message came
    getMessage(classLoader, key + ".message", args)
  }
}

sealed abstract class Message[+T <: FileSpec]()

case class StartWork[+T <: FileSpec]() extends Message[T]
case class EndWork[+T <: FileSpec]() extends Message[T]

case class StartFile[+T <: FileSpec](fileSpec: T) extends Message[T]
case class EndFile[+T <: FileSpec](fileSpec: T) extends Message[T]

case class StyleError[+T <: FileSpec](fileSpec: T, clazz: Class[_ <: Checker[_]], key: String,
                                      level: Level, args: List[String], lineNumber: Option[Int] = None, column: Option[Int] = None) extends Message[T] {
  override def toString() = "key=" + key + " args=" + args + " lineNumber=" + lineNumber + " column=" + column
}
case class StyleException[+T <: FileSpec](fileSpec: T, clazz: Class[_ <: Checker[_]], message: String,
                                          stacktrace: String, lineNumber: Option[Int] = None, column: Option[Int] = None) extends Message[T]

sealed abstract class ScalastyleError
case class PositionError(position: Int, args: List[String] = List[String]()) extends ScalastyleError
case class FileError(args: List[String] = List[String]()) extends ScalastyleError
case class LineError(line: Int, args: List[String] = List[String]()) extends ScalastyleError
case class ColumnError(line: Int, column: Int, args: List[String] = List[String]()) extends ScalastyleError

