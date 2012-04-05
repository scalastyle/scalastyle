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

class Main
case class MainConfig(config: Option[String], directories: List[String],
                        verbose: Boolean = false, quiet: Boolean = false,
                        warningsaserrors: Boolean = false)

object Main {
  def main(args: Array[String]): Unit = {
    val parser = new scopt.immutable.OptionParser[MainConfig]("scalastyle", "0.1.0") {
      def options = Seq(
        opt("c", "config", "configuration file (required)") { (v: String, c: MainConfig) => c.copy(config = Some(v)) },
        booleanOpt("v", "verbose", "verbose") { (v: Boolean, c: MainConfig) => c.copy(verbose = v) },
        booleanOpt("q", "quiet", "quiet") { (v: Boolean, c: MainConfig) => c.copy(quiet = v) },
        booleanOpt("w", "warnings", "fail if there are warnings") { (v: Boolean, c: MainConfig) => c.copy(warningsaserrors = v) },
        arglist("<directory>", "directories / files") { (v: String, c: MainConfig) => c.copy(directories = v :: c.directories) })
    }

    // parser.parse returns Option[C]
    val exitVal = parser.parse(args, MainConfig(None, List())) map { config =>
      if (!config.config.isDefined || config.directories.size == 0) {
        parser.showUsage
        1
      } else {
        if (execute(config)) 1 else 0
      }
    } getOrElse {
      // arguments are bad, usage message will have been displayed
      1
    }

    System.exit(exitVal)
  }

  private[this] def now(): Long = new Date().getTime()

  private[this] def execute(config: MainConfig): Boolean = {
    val start = now()
    val configuration = ScalastyleConfiguration.readFromXml(config.config.get)
    val messages = new ScalastyleChecker().checkFiles(configuration, Directory.getFiles(config.directories.map(s => new File(s)): _*))

    val outputResult = new TextOutput().output(messages)
    if (!config.quiet) println("Processed " + outputResult.files + " file(s)")
    if (!config.quiet) println("Found " + outputResult.errors + " errors")
    if (!config.quiet) println("Found " + outputResult.warnings + " warnings")
    if (!config.quiet) println("Finished in " + (now - start) + " ms")

    outputResult.errors > 0 || (config.warningsaserrors && outputResult.warnings > 0)
  }
}
