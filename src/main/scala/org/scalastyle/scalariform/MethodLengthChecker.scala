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

package org.scalastyle.scalariform

import org.scalastyle.CombinedMeta
import org.scalastyle.CombinedMetaChecker
import org.scalastyle.ScalastyleError

import scala.meta.Defn

class MethodLengthChecker extends CombinedMetaChecker {
  val errorKey = "method.length"
  val DefaultMaximumLength = 50
  val DefaultIgnoreComments = false

  def verify(ast: CombinedMeta): List[ScalastyleError] = {
    val maxLength = getInt("maxLength", DefaultMaximumLength)
    val ignoreComments = getBoolean("ignoreComments", DefaultIgnoreComments)

    val defs = SmVisitor.getAll[Defn.Def](ast.tree).filter(newMatches(maxLength, ignoreComments))

    defs.map(d => toError(d.name, List("" + maxLength)))
  }

  private def newMatches(maxLines: Int, ignoreComments: Boolean)(d: Defn.Def): Boolean = {
    if (ignoreComments) {
      val count = SmVisitor.countNewLines(d)

      count > maxLines
    } else {
      d.pos.endLine - d.pos.startLine > maxLines
    }
  }
}
