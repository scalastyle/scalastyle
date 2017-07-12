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
import _root_.scalariform.parser.{CompilationUnit, Param, ParamClauses}
import scala.util.matching.Regex

// scalastyle:off multiple.string.literals

class ClassNamesChecker extends ScalariformChecker {
  val DefaultRegex = "^[A-Z][A-Za-z]*$"
  val DefaultMatchCondition = true
  val errorKey = "class.name"

  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val regexString = getString("regex", DefaultRegex)
    val regex = regexString.r
    val matchCondition = getBoolean("match", DefaultMatchCondition)

    val it = for {
      List(left, right) <- ast.tokens.sliding(2)
      if left.tokenType == CLASS && matchesBasedOnCondition(regex, right.text, matchCondition)
    } yield {
      PositionError(right.offset, List(regexString))
    }

    it.toList
  }
}

class ObjectNamesChecker extends ScalariformChecker {
  val DefaultRegex = "^[A-Z][A-Za-z]*$"
  val DefaultMatchCondition = true
  val errorKey = "object.name"

  def verify(ast: CompilationUnit): List[PositionError] = {
    val regexString = getString("regex", DefaultRegex)
    val regex = regexString.r
    val matchCondition = getBoolean("match", DefaultMatchCondition)

    val it = for {
      List(left, middle, right) <- ast.tokens.sliding(3)
      if left.tokenType != PACKAGE && middle.tokenType == OBJECT &&
        matchesBasedOnCondition(regex, right.text, matchCondition)
    } yield {
      PositionError(right.offset, List(regexString))
    }

    it.toList
  }
}

class PackageNamesChecker extends ScalariformChecker {
  val DefaultRegex = "^[a-z][A-Za-z]*$"
  val DefaultMatchCondition = true
  val errorKey = "package.name"

  def verify(ast: CompilationUnit): List[PositionError] = {
    val regexString = getString("regex", DefaultRegex)
    val regex = regexString.r
    val matchCondition = getBoolean("match", DefaultMatchCondition)

    def isPartOfPackageName(t: Token): Boolean = (t.tokenType == DOT) || (t.tokenType == VARID)

    @annotation.tailrec
    def getNextPackageName(tokens: List[Token]): (List[Token], List[Token]) = tokens match {
      case Nil => (Nil, Nil)
      case hd :: tail if hd.tokenType == PACKAGE => tail.span(isPartOfPackageName)
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
      if matchesBasedOnCondition(regex, pkgName.text, matchCondition)
    } yield {
      PositionError(pkgName.offset, List(regexString))
    }

    it
  }
}

class PackageObjectNamesChecker extends ScalariformChecker {
  val DefaultRegex = "^[a-z][A-Za-z]*$"
  val DefaultMatchCondition = true
  val errorKey = "package.object.name"

  def verify(ast: CompilationUnit): List[PositionError] = {
    val regexString = getString("regex", DefaultRegex)
    val regex = regexString.r
    val matchCondition = getBoolean("match", DefaultMatchCondition)

    val it = for {
      List(left, middle, right) <- ast.tokens.sliding(3)
      if left.tokenType == PACKAGE && middle.tokenType == OBJECT &&
        matchesBasedOnCondition(regex, right.text, matchCondition)
    } yield {
      PositionError(right.offset, List(regexString))
    }

    it.toList
  }
}

case class MethodNamesCheckerParameters(regexString: String, ignoreRegexString: String, ignoreOverride: Boolean,
                                        matchCondition: Boolean) {
  private val regexR = regexString.r
  private val ignoreRegexR = ignoreRegexString.r

  def regex(): Regex = regexR
  def ignoreRegex(): Regex = ignoreRegexR
}

case class MethodArgumentNamesCheckerParameters(regexString: String, ignoreRegexString: String) {
  private val regexR = regexString.r
  private val ignoreRegexR = ignoreRegexString.r

  def regex(): Regex = regexR
  def ignoreRegex(): Regex = ignoreRegexR
}

class MethodNamesChecker extends AbstractSingleMethodChecker[MethodNamesCheckerParameters] {
  private val DefaultRegex = "^[a-z][A-Za-z0-9]*(_=)?$"
  private val DefaultIgnoreRegex = "^$"
  private val DefaultIgnoreOverride = false
  private val DefaultMatchCondition = true
  val errorKey = "method.name"

  protected def matchParameters(): MethodNamesCheckerParameters = {
    MethodNamesCheckerParameters(getString("regex", DefaultRegex), getString("ignoreRegex", DefaultIgnoreRegex),
        getBoolean("ignoreOverride", DefaultIgnoreOverride), getBoolean("match", DefaultMatchCondition))
  }

  protected def matches(t: FullDefOrDclVisit, p: MethodNamesCheckerParameters): Boolean = {
    if (p.ignoreOverride && isOverride(t.fullDefOrDcl.modifiers)) {
      false
    } else {
      val name = t.funDefOrDcl.nameToken.text
      !matches(p.regex(), name, p.matchCondition) && !matches(p.ignoreRegex(), name, p.matchCondition)
    }
  }

  protected override def describeParameters(p: MethodNamesCheckerParameters) = List("" + p.regex)

  private def matches(regex: Regex, s: String, matchCondition: Boolean) =
    regex.findFirstIn(s).isDefined && matchCondition
}

class MethodArgumentNamesChecker extends AbstractSingleMethodChecker[MethodArgumentNamesCheckerParameters] {
  private val DefaultRegex = "^[a-z][A-Za-z0-9]*$"
  private val DefaultIgnoreRegex = "^$"
  val errorKey: String = "method.argument.name"

  def matchParameters(): MethodArgumentNamesCheckerParameters = {
    MethodArgumentNamesCheckerParameters(getString("regex", DefaultRegex), getString("ignoreRegex", DefaultIgnoreRegex))
  }

  def matches(t: FullDefOrDclVisit, p: MethodArgumentNamesCheckerParameters): Boolean = {
    getParams(t.funDefOrDcl.paramClauses) match {
      case Nil => false
      case params: List[Param] => params.exists { pc =>
        val name = pc.id.text
        !matches(p.ignoreRegex(), name) && !matches(p.regex(), name)
      }
    }
  }

  private def getParams(p: ParamClauses): List[Param] = {
    p.paramClausesAndNewlines.map(_._1).flatMap(pc => pc.firstParamOption :: pc.otherParams.map(p => Some(p._2))).flatten
  }

  protected override def describeParameters(p: MethodArgumentNamesCheckerParameters) = List("" + p.regex)

  private def matches(regex: Regex, s: String) = regex.findFirstIn(s).isDefined
}

class FieldNamesChecker extends ScalariformChecker {
  val DefaultRegex = "^[a-z][A-Za-z0-9]*$"
  val errorKey = "field.name"
  val DefaultMatchCondition = true

  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val regexString = getString("regex", DefaultRegex)
    val regex = regexString.r
    val matchCondition = getBoolean("match", DefaultMatchCondition)

    val it = for {
      List(left, right) <- ast.tokens.sliding(2)
      if (left.tokenType == VAL || left.tokenType == VAR) && matchesBasedOnCondition(regex, right.text, matchCondition)
    } yield {
      PositionError(right.offset, List(regexString))
    }

    it.toList
  }
}
