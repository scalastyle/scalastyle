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

import _root_.scalariform.lexer.Token
import _root_.scalariform.lexer.Tokens.CLASS
import _root_.scalariform.lexer.Tokens.DOT
import _root_.scalariform.lexer.Tokens.OBJECT
import _root_.scalariform.lexer.Tokens.PACKAGE
import _root_.scalariform.lexer.Tokens.VAL
import _root_.scalariform.lexer.Tokens.VAR
import _root_.scalariform.lexer.Tokens.VARID
import _root_.scalariform.parser.CompilationUnit
import scala.util.matching.Regex

// scalastyle:off multiple.string.literals

class ClassNamesChecker extends ScalariformChecker {
  val DefaultRegex = "^[A-Z][A-Za-z]*$"
  val errorKey = "class.name"

  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val regexString = getString("regex", DefaultRegex)
    val regex = regexString.r

    val it = for {
      List(left, right) <- ast.tokens.sliding(2)
      if left.tokenType == CLASS && regex.findAllIn(right.text).isEmpty
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
      List(left, middle, right) <- ast.tokens.sliding(3)
      if left.tokenType != PACKAGE && middle.tokenType == OBJECT && regex.findAllIn(right.text).isEmpty
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

    def isPartOfPackageName(t: Token): Boolean = (t.tokenType == DOT) || (t.tokenType == VARID)

    @annotation.tailrec
    def getNextPackageName(tokens: List[Token]): (List[Token], List[Token]) = tokens match {
      case Nil => (Nil, Nil)
      case hd :: tail if hd.tokenType == PACKAGE => tail.span(isPartOfPackageName(_))
      case l: Any => getNextPackageName(l.dropWhile(tok => tok.tokenType != PACKAGE))
    }

    @annotation.tailrec
    def getPackageNameLoop(tokens: List[Token], myAccumulator: List[List[Token]]): List[List[Token]] =
      getNextPackageName(tokens) match {
        case (Nil, Nil) => myAccumulator.reverse  // Return the result, but reverse since we gathered backward
        case (Nil, remainder) => getPackageNameLoop(remainder, myAccumulator) // Found package object - try again
        case (l, remainder) =>  // add match to results, go look again
          val pkgName = l.filter(tok => tok.tokenType != DOT) // Strip out the dots between varids
          getPackageNameLoop(remainder, pkgName :: myAccumulator)
      }

    val packageNames = getPackageNameLoop(ast.tokens, Nil)

    val it = for {
      pkgName <- packageNames.flatten
      if regex.findAllIn(pkgName.text).isEmpty
    } yield {
      PositionError(pkgName.offset, List(regexString))
    }

    it
  }
}

class PackageObjectNamesChecker extends ScalariformChecker {
  val DefaultRegex = "^[a-z][A-Za-z]*$"
  val errorKey = "package.object.name"

  def verify(ast: CompilationUnit): List[PositionError] = {
    val regexString = getString("regex", DefaultRegex)
    val regex = regexString.r

    val it = for {
      List(left, middle, right) <- ast.tokens.sliding(3)
      if left.tokenType == PACKAGE && middle.tokenType == OBJECT && regex.findAllIn(right.text).isEmpty
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
      !matches(p.regex(), name) && !matches(p.ignoreRegex(), name)
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
      if (left.tokenType == VAL || left.tokenType == VAR) && regex.findAllIn(right.text).isEmpty
    } yield {
      PositionError(right.offset, List(regexString))
    }

    it.toList
  }
}
