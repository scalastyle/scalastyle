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

import scalariform.parser.AccessModifier
import scalariform.parser.Modifier
import scalariform.parser.Param
import scalariform.parser.ParamClauses
import scalariform.parser.ProcFunBody
import scalariform.lexer.Tokens

class ProcedureDeclarationChecker extends AbstractSingleMethodChecker[Unit] {
  val errorKey = "procedure.declaration"

  protected def matchParameters() = Unit

  protected def matches(t: FullDefOrDclVisit, p: Unit) = {
    val x = t.funDefOrDcl.funBodyOpt match {
      // match if we don't have a body, and there is no return type
      case None => !t.funDefOrDcl.returnTypeOpt.isDefined
      // match if we do have a body, and the first character is not equals
      case Some(x) => (x.tokens.size > 0 && x.tokens(0).tokenType != Tokens.EQUALS)
      case _ => false
    }
    x
  }
}
