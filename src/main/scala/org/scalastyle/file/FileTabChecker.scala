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

import scala.Array.canBuildFrom

import org.scalastyle.ColumnError
import org.scalastyle.FileChecker
import org.scalastyle.Lines
import org.scalastyle.ScalastyleError

class FileTabChecker extends FileChecker {
  val errorKey = "line.contains.tab"

  def verify(lines: Lines): List[ScalastyleError] = {
    val errors = for {
      line <- lines.lines.zipWithIndex;
      if line._1.text.contains('\t')
    } yield {
      ColumnError(line._2 + 1, line._1.text.indexOf('\t'))
    }

    errors.toList
  }
}
