package org.segl.scalastyle

import java.text.MessageFormat
import java.util.ResourceBundle
import java.util.Locale
import scala.collection.mutable.HashMap

trait FileSpec {
  def name: String
}

class MessageHelper {
  val bundles = HashMap[String, ResourceBundle]()

  def getMessage(clazz: Class[_ <: Checker[_]], key: String, args: List[String]) = {
    try {
      val packageName = clazz.getPackage().getName()
      val bundle = bundles.synchronized(bundles.getOrElseUpdate(packageName, {
        ResourceBundle.getBundle(packageName + ".messages", Locale.getDefault(), clazz.getClassLoader())
      }))

      // Use ClassLoader of the class from which the message came
      val pattern = bundle.getString(key);
      MessageFormat.format(pattern, args.map(_.asInstanceOf[AnyRef]): _*);
    } catch {
      // If there is no message, just use the key
      case _ => MessageFormat.format(key, args.map(_.asInstanceOf[AnyRef]): _*)
    }
  }
}

sealed abstract class Message[+T <: FileSpec]()

case class StartWork[+T <: FileSpec]() extends Message[T]
case class EndWork[+T <: FileSpec]() extends Message[T]

case class StartFile[+T <: FileSpec](fileSpec: T) extends Message[T]
case class EndFile[+T <: FileSpec](fileSpec: T) extends Message[T]

case class StyleError[+T <: FileSpec](fileSpec: T, clazz: Class[_ <: Checker[_]], key: String, args: List[String], lineNumber: Option[Int] = None, column: Option[Int] = None) extends Message[T] {
  override def toString() = "key=" + key + " args=" + args + " lineNumber=" + lineNumber + " column=" + column 
}
case class StyleException[+T <: FileSpec](fileSpec: T, clazz: Class[_ <: Checker[_]], message: String, stacktrace: String, lineNumber: Option[Int] = None, column: Option[Int] = None) extends Message[T]

sealed abstract class ScalastyleError
case class PositionError(position: Int, args: List[String] = List[String]()) extends ScalastyleError
case class FileError(args: List[String] = List[String]()) extends ScalastyleError
case class LineError(line: Int, args: List[String] = List[String]()) extends ScalastyleError
case class ColumnError(line: Int, column: Int, args: List[String] = List[String]()) extends ScalastyleError

