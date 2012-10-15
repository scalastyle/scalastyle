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

package org.scalastyle.scalariform;

import org.scalastyle.PositionError
import org.scalastyle.ScalariformChecker
import org.scalastyle.ScalastyleError

import VisitorHelper.visit
import scalariform.lexer.Tokens.LBRACE
import scalariform.lexer.Tokens.RBRACE
import scalariform.parser.AstNode
import scalariform.parser.CompilationUnit
import scalariform.parser.TmplDef
import scalariform.parser.TypeParamClause
import scalariform.parser.TypeParam
import scalariform.parser.GeneralTokens
import scalariform.parser.VarianceTypeElement
import scala.util.matching.Regex

class EmptyClassChecker extends AbstractClassChecker {
  val errorKey = "empty.class"

  private def isEmptyBlock(ast: AstNode): Boolean = {
    ast.tokens.size == 2 && ast.tokens(0).tokenType == LBRACE && ast.tokens(1).tokenType == RBRACE
  }

  def matches(t: TmplClazz): Boolean = {
    t.t.templateBodyOption match {
      case None => false
      case Some(tbo) => isEmptyBlock(tbo)
    }
  }
}

class ClassTypeParameterChecker extends AbstractClassChecker {
  val DefaultRegex = "^[A-Z_]$"
  val errorKey = "class.type.parameter.name"

  private[this] def matches(t: TypeParamClause): Boolean = {
    val regexString = getString("regex", DefaultRegex)
    val regex = regexString.r

    t.contents.map(c => innermostName(c)).flatten.exists(s => !matchesRegex(regex, s))
  }

  private[this] def matchesRegex(regex: Regex, s: String) = (regex findAllIn (s)).size == 1

  private[this] def innermostName(ast: Any): Option[String] = {
    ast match {
      case typeParam: TypeParam => {
        typeParam.contents match {
          case List(GeneralTokens(list)) => Some(list(0).text)
          case List(GeneralTokens(list), TypeParamClause(x)) => innermostName(x(1))
          case VarianceTypeElement(_) :: GeneralTokens(list) :: Nil => Some(list(0).text)
          case GeneralTokens(list) :: tail => Some(list(0).text)
          case VarianceTypeElement(_) :: GeneralTokens(list) :: tail => Some(list(0).text)
          case _ => None
        }
      }
      case _ => None
    }
  }

  def matches(t: TmplClazz): Boolean = {
    t.t.typeParamClauseOpt match {
      case None => false
      case Some(tbo) => matches(tbo)
    }
  }
}
