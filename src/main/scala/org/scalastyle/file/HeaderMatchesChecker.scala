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

package org.scalastyle.file

import org.scalastyle.Checker
import org.scalastyle.FileChecker
import org.scalastyle.FileError
import org.scalastyle.LineError
import org.scalastyle.Lines
import org.scalastyle.ScalastyleError

class HeaderMatchesChecker extends FileChecker {
  val errorKey = "header.matches"
  val DefaultHeader = ""

  def verify(ast: Lines): List[ScalastyleError] = {
    val regexParameter = getBoolean("regex", false)
    val headerParameter = getString("header", DefaultHeader)
    if (regexParameter) {

      // Convert any Windows line termination sequences ("\r\n") to Unix/Linux/BSD style for consistency.
      val Regex = (headerParameter.replaceAll("\r\n", "\n") ++ "(?s:.*)").r
      val fullSource = ast.lines map { _.text } mkString "\n"
      fullSource match {
        case Regex() =>
          List()
        case _ =>
          List(FileError())
      }
    } else {
      val header = Checker.parseLines(headerParameter)
      val found = (0 until scala.math.min(ast.lines.length, header.lines.length)).find(i =>
        !ast.lines(i).text.equals(header.lines(i).text)
      )

      found match {
        case Some(x) => List(LineError(x + 1))
        case None => {
          if (ast.lines.size < header.lines.length) {
            List(LineError(ast.lines.length))
          } else {
            List()
          }
        }
      }
    }
  }
}
