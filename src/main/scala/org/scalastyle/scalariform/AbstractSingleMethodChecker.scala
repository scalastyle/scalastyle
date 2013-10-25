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

import org.scalastyle.scalariform.VisitorHelper.Clazz
import org.scalastyle.PositionError
import org.scalastyle.ScalariformChecker
import org.scalastyle.ScalastyleError
import scalariform.parser.CompilationUnit
import scalariform.parser.DefOrDcl
import scalariform.parser.FullDefOrDcl
import scalariform.parser.FunDefOrDcl
import VisitorHelper.{visit, Clazz}
import scalariform.parser.SimpleModifier
import scalariform.parser.Modifier
import scalariform.parser.AccessModifier
import scalariform.lexer.Tokens

abstract class AbstractSingleMethodChecker[T] extends ScalariformChecker {


  case class FullDefOrDclVisit(fullDefOrDcl: FullDefOrDcl, funDefOrDcl: FunDefOrDcl, subs: List[FullDefOrDclVisit]) extends Clazz[FullDefOrDcl]()

  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val p = matchParameters()

    val it = for {
      t <- localvisit(ast.immediateChildren(0));
      f <- traverse(t);
      if (matches(f, p))
    } yield {
      PositionError(f.funDefOrDcl.nameToken.offset, describeParameters(p))
    }

    it.toList
  }

  private def traverse(t: FullDefOrDclVisit): List[FullDefOrDclVisit] = t :: t.subs.map(traverse(_)).flatten

  protected def matchParameters(): T
  protected def matches(t: FullDefOrDclVisit, parameters: T): Boolean
  protected def describeParameters(parameters: T): List[String] = Nil

  private def localvisit(ast: Any): List[FullDefOrDclVisit] = ast match {
    case t: FullDefOrDcl => {
      t.defOrDcl match {
        case f: FunDefOrDcl => List(FullDefOrDclVisit(t, f, localvisit(f)))
        case _ => localvisit(t.defOrDcl)
      }
    }
    case t: FunDefOrDcl => localvisit(t.funBodyOpt)
    case t: Any => visit(t, localvisit)
  }

  protected def isOverride(modifiers: List[Modifier]) = modifiers.exists(_ match {
    case sm: SimpleModifier if (sm.token.text == "override") => true
    case _ => false
  })

  protected def privateOrProtected(modifiers: List[Modifier]) = modifiers.exists( _ match {
    case am: AccessModifier => true
    case _ => false
  })

  protected def isConstructor(defOrDcl: DefOrDcl) = defOrDcl match {
    case fun: FunDefOrDcl => fun.nameToken.tokenType == Tokens.THIS
    case _ => false
  }
}
