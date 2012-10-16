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

import org.scalastyle.FileChecker
import org.scalastyle.FileError
import org.scalastyle.Lines
import org.scalastyle.ScalastyleError

class FileLengthChecker extends FileChecker {
  val errorKey = "file.size.limit"
  val DefaultMaxFileLength = 1000

  def verify(ast: Lines): List[ScalastyleError] = {
    val maxFileLength = getInt("maxFileLength", DefaultMaxFileLength)

    if (ast.lines.size > maxFileLength) List(FileError(List("" + maxFileLength))) else List()
  }
}
