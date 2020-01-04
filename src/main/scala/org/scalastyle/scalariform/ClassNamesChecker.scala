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

import scala.util.matching.Regex

import _root_.scalariform.lexer.Token
import _root_.scalariform.lexer.Tokens.CLASS
import _root_.scalariform.lexer.Tokens.DOT
import _root_.scalariform.lexer.Tokens.LPAREN
import _root_.scalariform.lexer.Tokens.OBJECT
import _root_.scalariform.lexer.Tokens.PACKAGE
import _root_.scalariform.lexer.Tokens.VAL
import _root_.scalariform.lexer.Tokens.VAR
import _root_.scalariform.lexer.Tokens.VARID
import _root_.scalariform.parser.CompilationUnit
import _root_.scalariform.parser.FullDefOrDcl
import _root_.scalariform.parser.GeneralTokens
import _root_.scalariform.parser.Param
import _root_.scalariform.parser.ParamClauses
import _root_.scalariform.parser.PatDefOrDcl
import _root_.scalariform.parser.TemplateBody
import _root_.scalariform.parser.TmplDef
import org.scalastyle.PositionError
import org.scalastyle.ScalariformChecker
import org.scalastyle.ScalastyleError
import scalariform.parser.TypeExprElement

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
      case Nil                                   => (Nil, Nil)
      case hd :: tail if hd.tokenType == PACKAGE => tail.span(isPartOfPackageName)
      case l: Any                                => getNextPackageName(l.dropWhile(tok => tok.tokenType != PACKAGE))
    }

    @annotation.tailrec
    def getPackageNameLoop(tokens: List[Token], myAccumulator: List[List[Token]]): List[List[Token]] =
      getNextPackageName(tokens) match {
        case (Nil, Nil) => myAccumulator.reverse // Return the result, but reverse since we gathered backward
        case (Nil, remainder) =>
          getPackageNameLoop(remainder, myAccumulator) // Found package object - try again
        case (l, remainder) => // add match to results, go look again
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

case class MethodNamesCheckerParameters(
  regexString: String,
  ignoreRegexString: String,
  ignoreOverride: Boolean
) {
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
  val errorKey = "method.name"

  protected def matchParameters(): MethodNamesCheckerParameters =
    MethodNamesCheckerParameters(
      getString("regex", DefaultRegex),
      getString("ignoreRegex", DefaultIgnoreRegex),
      getBoolean("ignoreOverride", DefaultIgnoreOverride)
    )

  protected def matches(t: FullDefOrDclVisit, p: MethodNamesCheckerParameters): Boolean = {
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

class MethodArgumentNamesChecker extends AbstractSingleMethodChecker[MethodArgumentNamesCheckerParameters] {
  private val DefaultRegex = "^[a-z][A-Za-z0-9]*$"
  private val DefaultIgnoreRegex = "^$"
  val errorKey: String = "method.argument.name"

  def matchParameters(): MethodArgumentNamesCheckerParameters =
    MethodArgumentNamesCheckerParameters(
      getString("regex", DefaultRegex),
      getString("ignoreRegex", DefaultIgnoreRegex)
    )

  def matches(t: FullDefOrDclVisit, p: MethodArgumentNamesCheckerParameters): Boolean = {
    getParams(t.funDefOrDcl.paramClauses) match {
      case Nil => false
      case params: List[Param] =>
        params.exists { pc =>
          val name = pc.id.text
          !matches(p.ignoreRegex(), name) && !matches(p.regex(), name)
        }
    }
  }

  private def getParams(p: ParamClauses): List[Param] =
    p.paramClausesAndNewlines
      .map(_._1)
      .flatMap(pc => pc.firstParamOption :: pc.otherParams.map(p => Some(p._2)))
      .flatten

  protected override def describeParameters(p: MethodArgumentNamesCheckerParameters) = List("" + p.regex)

  private def matches(regex: Regex, s: String) = regex.findFirstIn(s).isDefined
}

class FieldNamesChecker extends ScalariformChecker {
  val DefaultRegex = "^[a-z][A-Za-z0-9]*$"
  val DefaultObjectFieldRegex = "^[A-Z][A-Za-z0-9]*$"
  val errorKey = "field.name"

  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val regex = getString("regex", DefaultRegex).r
    val objectFieldRegex = getString("objectFieldRegex", DefaultObjectFieldRegex).r

    localVisit(regex, objectFieldRegex, inValDef = false)(ast.immediateChildren.head)
  }

  private def localVisit(regex: Regex, objectFieldRegex: Regex, inValDef: Boolean)(
    ast: Any
  ): List[ScalastyleError] = {
    ast match {
      //object Name { ... }
      case TmplDef(List(Token(OBJECT, _, _, _)), _, _, _, _, _, _, Some(TemplateBody(_, _, stats, _))) =>
        //go through all first-level val declarations and apply objectFieldRegex
        stats.immediateChildren.flatMap(stat =>
          stat match {
            case FullDefOrDcl(_, _, PatDefOrDcl(Token(tokenType, _, _, _), expr, _, _, _))
                if tokenType == VAL || tokenType == VAR =>
              VisitorHelper.visit(expr, localVisit(objectFieldRegex, objectFieldRegex, inValDef = true))
            case t =>
              VisitorHelper.visit(t, localVisit(regex, objectFieldRegex, inValDef))
          }
        )

      //val ... =
      case PatDefOrDcl(Token(tokenType, _, _, _), expr, _, _, _) if tokenType == VAL || tokenType == VAR =>
        VisitorHelper.visit(expr, localVisit(regex, objectFieldRegex, inValDef = true))

      // don't descend into type elements
      case tee: TypeExprElement => Nil

      //destructuring start - val name(...
      case GeneralTokens(List(Token(VARID, _, _, _), Token(LPAREN, _, _, _))) if inValDef => Nil

      //actual name check
      case GeneralTokens(List(Token(VARID, name, offset, _))) if inValDef && regex.findAllIn(name).isEmpty =>
        List(PositionError(offset, List(regex.toString)))

      case t: Any =>
        VisitorHelper.visit(t, localVisit(regex, objectFieldRegex, inValDef))
    }
  }
}
