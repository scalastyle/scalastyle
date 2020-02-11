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

import _root_.scalariform.lexer.Tokens
import _root_.scalariform.parser.AccessModifier
import _root_.scalariform.parser.CompilationUnit
import _root_.scalariform.parser.DefOrDcl
import _root_.scalariform.parser.FullDefOrDcl
import _root_.scalariform.parser.FunDefOrDcl
import _root_.scalariform.parser.Modifier
import _root_.scalariform.parser.PatDefOrDcl
import _root_.scalariform.parser.SimpleModifier
import org.scalastyle.PositionError
import org.scalastyle.ScalariformChecker
import org.scalastyle.ScalastyleError
import org.scalastyle.scalariform.VisitorHelper.Clazz
import org.scalastyle.scalariform.VisitorHelper.visit

abstract class AbstractSingleMethodChecker[T] extends ScalariformChecker {

  case class FullDefOrDclVisit(
    fullDefOrDcl: FullDefOrDcl,
    funDefOrDcl: FunDefOrDcl,
    subs: List[FullDefOrDclVisit],
    insideDefOrValOrVar: Boolean
  ) extends Clazz[FullDefOrDcl]()

  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val p = matchParameters()

    val it = for {
      t <- localvisit(insideDefOrValOrVar = false)(ast.immediateChildren.head)
      f <- traverse(t)
      if matches(f, p)
    } yield {
      PositionError(f.funDefOrDcl.nameToken.offset, describeParameters(p))
    }

    it
  }

  private def traverse(t: FullDefOrDclVisit): List[FullDefOrDclVisit] = t :: t.subs.flatMap(traverse)

  protected def matchParameters(): T
  protected def matches(t: FullDefOrDclVisit, parameters: T): Boolean
  protected def describeParameters(parameters: T): List[String] = Nil

  private def localvisit(insideDefOrValOrVar: Boolean)(ast: Any): List[FullDefOrDclVisit] = ast match {
    case t: FullDefOrDcl => {
      t.defOrDcl match {
        case f: FunDefOrDcl => List(FullDefOrDclVisit(t, f, localvisit(true)(f), insideDefOrValOrVar))
        case f: PatDefOrDcl => localvisit(true)(f.equalsClauseOption)
        case _              => localvisit(insideDefOrValOrVar)(t.defOrDcl)
      }
    }
    case t: FunDefOrDcl => localvisit(true)(t.funBodyOpt)
    case t: Any         => visit(t, localvisit(insideDefOrValOrVar))
  }

  protected def isOverride(modifiers: List[Modifier]) = modifiers.exists {
    case sm: SimpleModifier if sm.token.text == "override" => true
    case _                                                 => false
  }

  protected def privateOrProtected(modifiers: List[Modifier]) = modifiers.exists {
    case am: AccessModifier => true
    case _                  => false
  }

  protected def isConstructor(defOrDcl: DefOrDcl) = defOrDcl match {
    case fun: FunDefOrDcl => fun.nameToken.tokenType == Tokens.THIS
    case _                => false
  }
}
