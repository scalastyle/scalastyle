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

import org.scalastyle.ScalametaChecker
import org.scalastyle.ScalastyleError

import scala.meta.Lit
import scala.meta.Term
import scala.meta.Tree
import scala.meta.tokens.Token

class NullChecker extends ScalametaChecker {
  val errorKey = "null"
  val dummyToken: Option[Token] = None

  val defaultAllowNullChecks = true
  lazy val allowNullChecks: Boolean = getBoolean("allowNullChecks", defaultAllowNullChecks)

  final def verify(ast: Tree): List[ScalastyleError] = {
    val it = for {
      n <- SmVisitor.getAll[Lit.Null](ast)
      if !allowNullChecks || !isCheck(n.parent)
    } yield toError(n)

    it
  }

  def isCheck(prev: Option[Tree]): Boolean = {
    prev match {
      case Some(x: Term.ApplyInfix) if x.op.value == "==" || x.op.value == "!=" => true
      case _ => false
    }
  }
}
