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

case class PublicMethodsHaveTypeParameters(ignoreOverride: Boolean)

class PublicMethodsHaveTypeChecker extends AbstractSingleMethodChecker[PublicMethodsHaveTypeParameters] {
  val errorKey = "public.methods.have.type"

  protected def matchParameters() = PublicMethodsHaveTypeParameters(getBoolean("ignoreOverride", false))

  protected def matches(t: FullDefOrDclVisit, p: PublicMethodsHaveTypeParameters) = {
    t.funDefOrDcl.funBodyOpt match {
      case Some(ProcFunBody(newlineOpt, bodyBlock)) => false
      case None =>
        // When funBodyOpt is None, it is assumed to be a declaration of a procedure.
        // Unit return type is not required.
        false
      case _ => t.funDefOrDcl.returnTypeOpt.isEmpty && !privateOrProtected(t.fullDefOrDcl.modifiers) &&
                           !isConstructor(t.fullDefOrDcl.defOrDcl) &&
                           !(p.ignoreOverride && isOverride(t.fullDefOrDcl.modifiers))
    }
  }
}
