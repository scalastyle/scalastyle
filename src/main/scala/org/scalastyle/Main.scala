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

import java.io.File;
import java.util.Date;
import scala.io.Codec;

class Main
case class MainConfig(error: Boolean, config: Option[String] = None, directories: List[String] = List(),
  verbose: Boolean = false, quiet: Boolean = false,
  warningsaserrors: Boolean = false, xmlFile: Option[String] = None,
  xmlEncoding: Option[String] = None, inputEncoding: Option[String] = None)

object Main {
  // scalastyle:off regex
  private def usage(version: String) = {
    println("scalastyle " + version)
    println("Usage: scalastyle [options] <source directory>")
    println(" -c, --config FILE               configuration file (required)")
    println(" -v, --verbose true|false        verbose output")
    println(" -q, --quiet true|false          be quiet")
    println("     --xmlOutput FILE            write checkstyle format output to this file")
    println("     --xmlEncoding STRING        encoding to use for the xml file")
    println("     --inputEncoding STRING      encoding for the source files")
    println(" -w, --warnings true|false       fail if there are warnings")
    System.exit(1)
  }
  // scalastyle:on regex

  private def isTrue(s: String) = "true" equalsIgnoreCase s

  def parseArgs(args: Array[String], version: String) = {
    var config = MainConfig(false)
    var i = 0
    while (i < args.length) {
      if (args(i).startsWith("-") && i < args.length - 1) {
        args(i) match {
          case ("-c" | "--config") => config = config.copy(config = Some(args(i + 1)))
          case ("-v" | "--verbose") => config = config.copy(verbose = isTrue(args(i + 1)))
          case ("-q" | "--quiet") => config = config.copy(quiet = isTrue(args(i + 1)))
          case ("-w" | "--warnings") => config = config.copy(warningsaserrors = isTrue(args(i + 1)))
          case ("--xmlOutput") => config = config.copy(xmlFile = Some(args(i + 1)))
          case ("--xmlEncoding") => config = config.copy(xmlEncoding = Some(args(i + 1)))
          case ("--inputEncoding") => config = config.copy(inputEncoding = Some(args(i + 1)))
          case _ => config = config.copy(error = true)
        }
        i = i + 2
      } else {
        config = config.copy(directories = args(i) :: config.directories)
        i = i + 1
      }
    }

    if (!config.config.isDefined || config.directories.size == 0) {
      config = config.copy(error = true)
    }

    config
  }

  def main(args: Array[String]): Unit = {
    val properties = new java.util.Properties();
    properties.load(this.getClass().getResourceAsStream("/version.properties"));

    val version = properties.getProperty("scalastyle.version")

    val a = Array("-v", "true")
    val config = parseArgs(a, version)

    val exitVal = {
      if (config.error) {
        usage(version)
        1
      } else {
        if (execute(config)) 1 else 0
      }
    }

    System.exit(exitVal)
  }

  private[this] def now(): Long = new Date().getTime()

  private[this] def execute(config: MainConfig)(implicit codec: Codec): Boolean = {
    val start = now()
    val configuration = ScalastyleConfiguration.readFromXml(config.config.get)
    val messages = new ScalastyleChecker().checkFiles(configuration, Directory.getFiles(config.inputEncoding, config.directories.map(new File(_)).toSeq))

    // scalastyle:off regex

    val outputResult = new TextOutput().output(messages)
    config.xmlFile match {
      case Some(x) => {
        val encoding = config.xmlEncoding.getOrElse(codec.charSet).toString
        XmlOutput.save(x, encoding, messages)
      }
      case None =>
    }

    if (!config.quiet) println("Processed " + outputResult.files + " file(s)")
    if (!config.quiet) println("Found " + outputResult.errors + " errors")
    if (!config.quiet) println("Found " + outputResult.warnings + " warnings")
    if (!config.quiet) println("Finished in " + (now - start) + " ms")

    // scalastyle:on regex

    outputResult.errors > 0 || (config.warningsaserrors && outputResult.warnings > 0)
  }
}
