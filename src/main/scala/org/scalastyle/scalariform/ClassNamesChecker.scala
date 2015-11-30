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

import org.scalastyle.PositionError
import org.scalastyle.ScalariformChecker
import org.scalastyle.ScalastyleError
import scalariform.lexer.Tokens.CLASS
import scalariform.lexer.Tokens.NEWLINE
import scalariform.lexer.Tokens.OBJECT
import scalariform.lexer.Tokens.PACKAGE
import scalariform.lexer.Tokens.VAL
import scalariform.lexer.Tokens.VAR
import scalariform.lexer.Tokens.VARID
import scalariform.lexer.Tokens.DOT
import scalariform.parser.CompilationUnit
import scala.util.matching.Regex
import scalariform.lexer.Token

// scalastyle:off multiple.string.literals

class ClassNamesChecker extends ScalariformChecker {
  val DefaultRegex = "^[A-Z][A-Za-z]*$"
  val errorKey = "class.name"

  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val regexString = getString("regex", DefaultRegex)
    val regex = regexString.r

    val it = for {
      List(left, right) <- ast.tokens.sliding(2);
      if (left.tokenType == CLASS && (regex findAllIn (right.text)).size == 0)
    } yield {
      PositionError(right.offset, List(regexString))
    }

    it.toList
  }
}

class ObjectNamesChecker extends ScalariformChecker {
  val DefaultRegex = "^[A-Z][A-Za-z]*$"
  val errorKey = "object.name"

  def verify(ast: CompilationUnit): List[PositionError] = {
    val regexString = getString("regex", DefaultRegex)
    val regex = regexString.r

    val it = for {
      List(left, middle, right) <- ast.tokens.sliding(3);
      if (left.tokenType != PACKAGE && middle.tokenType == OBJECT && (regex findAllIn (right.text)).size == 0)
    } yield {
      PositionError(right.offset, List(regexString))
    }

    it.toList
  }
}

class PackageNamesChecker extends ScalariformChecker {
  val DefaultRegex = "^[a-z][A-Za-z]*$"
  val errorKey = "package.name"

  def verify(ast: CompilationUnit): List[PositionError] = {
    val regexString = getString("regex", DefaultRegex)
    val regex = regexString.r

    sealed trait PackageNameStatus
    object NoPackage extends PackageNameStatus
    object PossiblePackage extends PackageNameStatus
    object AssemblingPackage extends PackageNameStatus

    type StateType = (List[List[Token]], List[Token], PackageNameStatus)

    def getPackageNames(ast: List[Token]): List[List[Token]] = {
      val startingState: StateType = (List[List[Token]](), List[Token](), NoPackage)

      /* This foldLeft implements a simple state machine to process the stream of tokens.
       * There are three states: NoPackage, PossiblePackage, and AssemblingPackage.
       */
      val packageNameList = ast.foldLeft(startingState) {
        (currentState: StateType, nextToken: Token) => currentState match {
          case (result, currentPackage, currentState) => currentState match {
            case NoPackage => 
              if (nextToken.tokenType == PACKAGE) (result, Nil, PossiblePackage)
              else (result, Nil, NoPackage)
            case PossiblePackage =>
              if (nextToken.tokenType == VARID) (result, List(nextToken), AssemblingPackage)
              else (result, Nil, NoPackage) // We ignore `package object`
            case AssemblingPackage =>
              if (nextToken.tokenType == VARID) (result, nextToken::currentPackage, AssemblingPackage)
              else if (nextToken.tokenType == DOT) (result, currentPackage, AssemblingPackage)
              else ((currentPackage.reverse)::result, Nil, NoPackage)
      }}}._1
        
      packageNameList.reverse   // accumulated them by adding to start of list
    }

    var packageNames = getPackageNames(ast.tokens)

    val it = for {
      pkgName <- packageNames.flatten;
      if ((regex findAllIn (pkgName.text)).size == 0)
    } yield {
      PositionError(pkgName.offset, List(regexString))
    }

    it.toList
  }
}

class PackageObjectNamesChecker extends ScalariformChecker {
  val DefaultRegex = "^[a-z][A-Za-z]*$"
  val errorKey = "package.object.name"

  def verify(ast: CompilationUnit): List[PositionError] = {
    val regexString = getString("regex", DefaultRegex)
    val regex = regexString.r

    val it = for {
      List(left, middle, right) <- ast.tokens.sliding(3);
      if (left.tokenType == PACKAGE && middle.tokenType == OBJECT && (regex findAllIn (right.text)).size == 0)
    } yield {
      PositionError(right.offset, List(regexString))
    }

    it.toList
  }
}

case class MethodNamesCheckerParameters(regexString: String, ignoreRegexString: String, ignoreOverride: Boolean) {
  private val regexR = regexString.r
  private val ignoreRegexR = ignoreRegexString.r

  def regex(): Regex = regexR
  def ignoreRegex(): Regex = ignoreRegexR
}

class MethodNamesChecker extends AbstractSingleMethodChecker[MethodNamesCheckerParameters] {
  private val DefaultRegex = "^[a-z][A-Za-z0-9]*(_=)?$"
  private val DefaultIgnoreRegex = "^$"
  private val DefaultIgnoreOverride = false
  val errorKey = "method.name"

  protected def matchParameters() = {
    MethodNamesCheckerParameters(getString("regex", DefaultRegex), getString("ignoreRegex", DefaultIgnoreRegex),
        getBoolean("ignoreOverride", DefaultIgnoreOverride))
  }

  protected def matches(t: FullDefOrDclVisit, p: MethodNamesCheckerParameters) = {
    if (p.ignoreOverride && isOverride(t.fullDefOrDcl.modifiers)) {
      false
    } else {
      val name = t.funDefOrDcl.nameToken.text
      (!matches(p.regex, name) && !matches(p.ignoreRegex, name))
    }
  }

  protected override def describeParameters(p: MethodNamesCheckerParameters) = List("" + p.regex)

  private def matches(regex: Regex, s: String) = regex.findFirstIn(s).isDefined
}

class FieldNamesChecker extends ScalariformChecker {
  val DefaultRegex = "^[a-z][A-Za-z0-9]*$"
  val errorKey = "field.name"

  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val regexString = getString("regex", DefaultRegex)
    val regex = regexString.r

    val it = for {
      List(left, right) <- ast.tokens.sliding(2)
      if (left.tokenType == VAL || left.tokenType == VAR) && (regex findAllIn right.text).size == 0
    } yield {
      PositionError(right.offset, List(regexString))
    }

    it.toList
  }
}
