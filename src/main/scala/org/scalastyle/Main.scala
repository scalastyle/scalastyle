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

import java.io.File
import java.util.Date
import scala.io.Codec
import com.typesafe.config.ConfigFactory
import java.net.URLClassLoader

case class MainConfig(error: Boolean,
    config: Option[String] = None,
    suppressions: Option[Seq[Suppression]] = None,
    directories: List[String] = List(),
    verbose: Boolean = false,
    quiet: Boolean = false,
    warningsaserrors: Boolean = false,
    xmlConfigFile: Option[String] = None,
    xmlEncoding: Option[String] = None,
    inputEncoding: Option[String] = None,
    externalJar: Option[String] = None,
    excludedFiles: Seq[String] = Nil) {
  import MainConfig._

  def howToExclude: HowToExclude = {
    if (xmlSuppressionsAndExcludes) {
      throw new IllegalStateException("Cannot have both a suppressions file and " +
        "a list of excluded files. See main's usage.")
    } else if (suppressions.isDefined) {
      SuppressionXml
    } else if (excludedFiles.nonEmpty) {
      ExcludeFiles
    } else {
      DoNotExclude
    }
  }

  def xmlSuppressionsAndExcludes: Boolean = excludedFiles.nonEmpty && suppressions.isDefined
}

object MainConfig {
  sealed trait HowToExclude
  object SuppressionXml extends HowToExclude
  object ExcludeFiles extends HowToExclude
  object DoNotExclude extends HowToExclude
}

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
    println(" -e, --externalJar FILE          jar containing custom rules")
    println(" -x, --excludedFiles STRING      regular expressions to exclude file paths (delimited by semicolons).")
    println("                                 Cannot be used in conjunction with suppressions files.")
    println(" -s, --suppressionsFile STRING   path to checkstyle-like suppressions xml file. For example, the")
    println("                                 following config would exclude scalastyle checks with class names")
    println("                                 containing regex2 from being run on files containing regex1. More than")
    println("                                 one suppress tag is allowed.")
    println("                                   <suppressions>")
    println("                                     <suppress files=\"regex1\" checks=\"regex2\"/>")
    println("                                   </suppressions>")

    System.exit(1)
  }
  // scalastyle:on regex

  private def isTrue(s: String) = "true" equalsIgnoreCase s

  def parseArgs(args: Array[String]): MainConfig = {
    var config = MainConfig(false)
    var i = 0
    while (i < args.length) {
      if (args(i).startsWith("-") && i < args.length - 1) {
        args(i) match {
          case ("-c" | "--config") => config = config.copy(config = Some(args(i + 1)))
          case ("-v" | "--verbose") => config = config.copy(verbose = isTrue(args(i + 1)))
          case ("-q" | "--quiet") => config = config.copy(quiet = isTrue(args(i + 1)))
          case ("-w" | "--warnings") => config = config.copy(warningsaserrors = isTrue(args(i + 1)))
          case ("--xmlOutput") => config = config.copy(xmlConfigFile = Some(args(i + 1)))
          case ("--xmlEncoding") => config = config.copy(xmlEncoding = Some(args(i + 1)))
          case ("--inputEncoding") => config = config.copy(inputEncoding = Some(args(i + 1)))
          case ("-e" | "--externalJar") => config = config.copy(externalJar = Some(args(i + 1)))
          case ("-x" | "--excludedFiles") => config = config.copy(excludedFiles = args(i + 1).split(";"))
          case ("-s" | "--suppressionsFile") => config = parseSuppressionFileInput(config, args(i + 1))
          case _ => config = config.copy(error = true)
        }
        i = i + 2
      } else {
        config = config.copy(directories = args(i) :: config.directories)
        i = i + 1
      }
    }

    if (!config.config.isDefined || config.directories.size == 0 || config.xmlSuppressionsAndExcludes) {
      config = config.copy(error = true)
    }

    config
  }

  def main(args: Array[String]): Unit = {

    val config = parseArgs(args)

    val exitVal = {
      if (config.error) {
        usage(BuildInfo.version)
        1
      } else {
        if (execute(config)) 1 else 0
      }
    }

    System.exit(exitVal)
  }

  private[this] def now(): Long = new Date().getTime()

  /**
   * Decide which files to check, and which checks to run on those files, and run them.
   * @param mc result of parsing the command line arguments.
   */
  private[this] def execute(mc: MainConfig)(implicit codec: Codec): Boolean = {
    val start = now()
    val configuration = ScalastyleConfiguration.readFromXml(mc.config.get)
    val classLoader = mc.externalJar.flatMap(j => Some(new URLClassLoader(Array(new java.io.File(j).toURI.toURL))))
    val filesToCheck = Directory.getFiles(mc.inputEncoding, mc.directories.map(new File(_)).toSeq, excludedFiles=mc.excludedFiles)

    val filesAndRules = mc.howToExclude match {
      case MainConfig.SuppressionXml => SuppressionParser.filesAndRulesAfterSuppressions(filesToCheck, configuration, mc.suppressions.get)
      case MainConfig.ExcludeFiles => filesToCheck.map(FileNameAndRules(_, configuration))
      case MainConfig.DoNotExclude => filesToCheck.map(FileNameAndRules(_, configuration))
    }

    val messages = new ScalastyleChecker(classLoader).completeAllFileChecks(filesAndRules)

    // scalastyle:off regex
    val config = ConfigFactory.load(classLoader.getOrElse(this.getClass.getClassLoader))
    val outputResult = new TextOutput(config, mc.verbose, mc.quiet).output(messages)
    mc.xmlConfigFile match {
      case Some(x) => {
        val encoding = mc.xmlEncoding.getOrElse(codec.charSet).toString
        XmlOutput.save(config, x, encoding, messages)
      }
      case None =>
    }

    if (!mc.quiet) println("Processed " + outputResult.files + " file(s)")
    if (!mc.quiet) println("Found " + outputResult.errors + " errors")
    if (!mc.quiet) println("Found " + outputResult.warnings + " warnings")
    if (!mc.quiet) println("Finished in " + (now - start) + " ms")

    // scalastyle:on regex

    outputResult.errors > 0 || (mc.warningsaserrors && outputResult.warnings > 0)
  }

  // Visible for testing
  def parseSuppressionFileInput(config: MainConfig, fileName: String): MainConfig = {
    SuppressionParser.parseFile(fileName) match {
      case Left(error) =>
        System.err.println(error.moreInfo)
        config.copy(error = true)
      case Right(suppressions) => config.copy(suppressions = Some(suppressions))
    }
  }
}
