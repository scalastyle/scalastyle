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

import scala.meta.Term

class EmptyInterpolatedStringChecker extends CombinedMetaChecker {
  val errorKey = "empty.interpolated.strings"
  private val typesSupportingVariables = Set("s", "f")

  def verify(ast: CombinedMeta): List[ScalastyleError] = {
    for {
      i <- SmVisitor.getAll[Term.Interpolate](ast.tree)
      if matches(i)
    } yield {
      toError(i)
    }
  }

  private def matches(i: Term.Interpolate): Boolean = {
    i.args.isEmpty && typesSupportingVariables.contains(i.prefix.toString)
  }
}
