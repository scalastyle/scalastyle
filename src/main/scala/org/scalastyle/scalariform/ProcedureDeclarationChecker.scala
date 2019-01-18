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

import scala.meta.Tree
import scala.meta.Type
import scala.meta.tokens.Token

class ProcedureDeclarationChecker extends AbstractSingleMethodChecker[Unit] {
  val errorKey = "procedure.declaration"

  protected def matchParameters(): Unit = Unit

  protected def matches(t: FullDefOrDclVisit, p: Unit): Boolean = {
    t match {
      case d: DefnDefVisit => !isEqualsBeforeBody(d.defnDef.body)
      case d: DeclDefVisit => {
        d.declDef.decltpe match {
          case t: Type.Name => t.toString == ""
          case _ => false
        }
      }
    }
  }

  private def isEqualsBeforeBody(body: Tree): Boolean = {
    var start = body.tokens.start - 1
    while (SmVisitor.isA(body.tokens.tokens(start), classOf[Token.Space])) {
      start = start -1
    }

    SmVisitor.isA(body.tokens.tokens(start), classOf[Token.Equals])
  }
}

