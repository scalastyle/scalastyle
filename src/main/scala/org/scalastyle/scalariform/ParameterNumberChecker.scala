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

import scalariform.parser.Param
import scalariform.parser.ParamClauses

class ParameterNumberChecker extends AbstractSingleMethodChecker[Int] {
  val errorKey = "parameter.number"
  val DefaultMaximumParameters = 8

  protected def matchParameters() = getInt("maxParameters", DefaultMaximumParameters)

  protected def matches(t: FullDefOrDclVisit, maxParameters: Int) = getParams(t.funDefOrDcl.paramClauses).size > maxParameters
  protected override def describeParameters(maxParameters: Int) = List("" + maxParameters)

  private def getParams(p: ParamClauses): List[Param] = {
    p.paramClausesAndNewlines.map(_._1).flatMap(pc => pc.firstParamOption :: pc.otherParams.map(p => Some(p._2))).flatten
  }
}
