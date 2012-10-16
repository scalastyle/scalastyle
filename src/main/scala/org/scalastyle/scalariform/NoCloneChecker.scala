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

import scalariform.parser.AstNode
import scalariform.parser.FunDefOrDcl

class NoCloneChecker extends AbstractMethodChecker {
  val errorKey = "no.clone"

  def matches(t: BaseClazz[AstNode]): Boolean = {
    t.subs.exists(matchFunDefOrDcl(_, isClone))
  }

  private def isClone(t: FunDefOrDcl): Boolean = methodMatch("clone", noParameter() _)(t)
}
