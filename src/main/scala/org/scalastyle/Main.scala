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
case class MainConfig(config: Option[String], directories: List[String],
                        verbose: Boolean = false, quiet: Boolean = false,
                        warningsaserrors: Boolean = false, xmlFile: Option[String] = None,
                        xmlEncoding: Option[String] = None, inputEncoding: Option[String] = None)

object Main {
  def main(args: Array[String]): Unit = {
    val properties = new java.util.Properties();
    properties.load(this.getClass().getResourceAsStream("/version.properties"));

    val parser = new scopt.OptionParser[MainConfig]("scalastyle") {
        head("scalastyle", properties.getProperty("scalastyle.version"))
        opt[String]('c', "config") action { (v: String, c: MainConfig) => c.copy(config = Some(v)) } text("configuration file (required)")
        opt[Boolean]('v', "verbose") action { (v: Boolean, c: MainConfig) => c.copy(verbose = v) } text("verbose")
        opt[Boolean]('q', "quiet") action { (v: Boolean, c: MainConfig) => c.copy(quiet = v) } text("quiet")
        opt[String]("xmlOutput") action { (v: String, c: MainConfig) => c.copy(xmlFile = Some(v)) } text("XML output (optional)")
        opt[String]("xmlEncoding") action { (v: String, c: MainConfig) => c.copy(xmlEncoding = Some(v)) } text("XML output encoding (optional)")
        opt[String]("inputEncoding") action { (v: String, c: MainConfig) => c.copy(inputEncoding = Some(v)) } text("Source file encoding (input) (optional)")
        opt[Boolean]('w', "warnings") action { (v: Boolean, c: MainConfig) => c.copy(warningsaserrors = v) } text("fail if there are warnings")
        arg[String]("<directory>") action { (v: String, c: MainConfig) => c.copy(directories = v :: c.directories) } text("directories / files")
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
