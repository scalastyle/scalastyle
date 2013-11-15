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

import _root_.scalariform.lexer.{HiddenTokens, Token}
import _root_.scalariform.parser._
import _root_.scalariform.parser.FunDefOrDcl
import _root_.scalariform.parser.TmplDef
import org.scalastyle._
import org.scalastyle.scalariform.VisitorHelper._
import org.scalastyle.CombinedAst
import scala.util.matching.Regex

/**
 * Checks that the ScalaDoc exists for all accessible members:
 * - classes, traits, case classes and objects
 * - methods
 * - vals, vars and types
 *
 * The ScalaDoc's structure must satisfy the parameter of the constructor in case of
 * case classes and classes, or the parameter of the methods. The ScalaDoc must include
 * the type parameters. Finally, the ScalaDoc must include return description for non-Unit
 * returning methods.
 */
class ScalaDocChecker extends CombinedChecker {
  protected val errorKey: String = "scaladoc"

  val skipPrivate = true
  val skipQualifiedPrivate = false
  val skipProtected = false
  val skipQualifiedProtected = false

  override def verify(ast: CombinedAst): List[ScalastyleError] = {
    localVisit(skip = false, HiddenTokens(Nil), ast.lines)(ast.compilationUnit.immediateChildren(0))
  }

  import ScalaDocChecker._

  /*
   * Finds the ScalaDoc hidden in the ``token``, falling back on ``fallback`` if ``token``
   * contains no ScalaDoc.
   *
   * This is useful when including access levels, which are not reported as part of the following
   * token. So,
   *
   * ```
   * &#47;**
   *  * Contains magic
   *  *&#47;
   * protected val foo = 5
   * ```
   * is interpreted as
   *
   * ``FullDefOrDcl`` -> ``PatDefOrDcl``, with the ScalaDoc attached to the ``FulDefOrDcl``, which
   * finds its way to us here in ``fallback``.
   */
  private def findScalaDoc(token: Token, fallback: HiddenTokens): Option[ScalaDoc] = {
    def toScalaDoc(ht: HiddenTokens): Option[ScalaDoc] = ht.rawTokens.find(_.isScalaDocComment).map(ScalaDoc.apply)
    
    toScalaDoc(token.associatedWhitespaceAndComments).orElse(toScalaDoc(fallback))
  }

  // parse the parameters and report errors for the parameters (constructor or method)
  private def paramErrors(line: Int, paramClausesOpt: Option[ParamClauses])(scalaDoc: ScalaDoc): List[ScalastyleError] = {
    val varidTokens = paramClausesOpt.map(_.tokens.filter(_.tokenType.name == "VARID")).getOrElse(Nil)
    val paramNames = for {
      (name, idx) <- varidTokens.zipWithIndex
      if idx % 2 == 0
    } yield name.text

    if (paramNames.size != scalaDoc.params.size) {
      // bad param sizes
      List(LineError(line, List(MalformedParams)))
    } else {
      if (!scalaDoc.params.forall(p => paramNames.exists(p.name ==))) List(LineError(line, List(MalformedParams)))
      else Nil
    }
  }

  // parse the type parameters and report errors for the parameters (constructor or method)
  private def tparamErrors(line: Int, tparamClausesOpt: Option[TypeParamClause])(scalaDoc: ScalaDoc): List[ScalastyleError] = {
    val tparamNames = tparamClausesOpt.map(_.tokens.filter(_.tokenType.name == "VARID").map(_.text)).getOrElse(Nil)

    if (tparamNames.size != scalaDoc.typeParams.size) {
      // bad param sizes
      List(LineError(line, List(MalformedTypeParams)))
    } else {
      if (!scalaDoc.typeParams.forall(tp => tparamNames.exists(tp.name ==))) List(LineError(line, List(MalformedTypeParams)))
      else Nil
    }
  }

  // parse the parameters and report errors for the return types
  private def returnErrors(line: Int, returnTypeOpt: Option[(Token, Type)])(scalaDoc: ScalaDoc): List[ScalastyleError] = {
    val needsReturn = returnTypeOpt.exists { case (_, tpe) => tpe.firstToken.text != "Unit" }

    if (needsReturn && !scalaDoc.returns.isDefined) {
      List(LineError(line, List(MalformedReturn)))
    } else {
      Nil
    }
  }

  /*
   * process the AST, picking up only the parts that are interesting to us, that is
   * - access modifiers
   * - classes, traits, case classes and objects
   * - methods
   * - vals, vars and types
   *
   * we do not bother descending down any further
   */
  private def localVisit(skip: Boolean, fallback: HiddenTokens, lines: Lines)(ast: Any): List[ScalastyleError] = ast match {
    case t: FullDefOrDcl      =>
      // private, private[xxx];
      // protected, protected[xxx];

      // check if we are going to include or skip depending on access modifier
      val accessModifier = t.modifiers.find {
        case AccessModifier(_, _) => true
        case _                    => false
      }
      val skip = accessModifier.exists {
        case AccessModifier(pop, Some(_)) =>
          if (pop.text == "private") skipQualifiedPrivate else skipQualifiedProtected
        case AccessModifier(pop, None) =>
          if (pop.text == "private") skipPrivate else skipProtected
        case _ =>
          false
      }

      // pick the ScalaDoc "attached" to the modifier, which actually means
      // ScalaDoc of the following member
      val scalaDocs = for {
        modifier <- t.modifiers
        token    <- modifier.tokens
        comment  <- token.associatedWhitespaceAndComments
        if comment.token.isScalaDocComment
      } yield comment

      // descend
      visit(t, localVisit(skip, HiddenTokens(scalaDocs), lines))
    case t: TmplDef      =>
      // trait Foo, trait Foo[A];
      // class Foo, class Foo[A](a: A);
      // case class Foo(), case class Foo[A](a: A);
      // object Foo;
      val (_, line) = lines.findLineAndIndex(t.firstToken.offset).get

      // we are checking parameters and type parameters
      val errors = if (skip) Nil else findScalaDoc(t.firstToken, fallback).
        map { scalaDoc =>
          paramErrors(line, t.paramClausesOpt)(scalaDoc) ++
          tparamErrors(line, t.typeParamClauseOpt)(scalaDoc)
        }.getOrElse(List(LineError(line, List(Missing))))

      // and we descend, because we're interested in seeing members of the types
      errors ++ visit(t, localVisit(skip, fallback, lines))
    case t: FunDefOrDcl  =>
      // def foo[A, B](a: Int): B = ...
      val (_, line) = lines.findLineAndIndex(t.firstToken.offset).get

      // we are checking parameters, type parameters and returns
      val errors = if (skip) Nil else findScalaDoc(t.firstToken, fallback).
        map { scalaDoc =>
          paramErrors(line, Some(t.paramClauses))(scalaDoc) ++
          tparamErrors(line, t.typeParamClauseOpt)(scalaDoc) ++
          returnErrors(line, t.returnTypeOpt)(scalaDoc)
        }.
        getOrElse(List(LineError(line, List(Missing))))

      // we don't descend any further
      errors
    case t: TypeDefOrDcl =>
      // type Foo = ...
      val (_, line) = lines.findLineAndIndex(t.firstToken.offset).get

      // error is non-existence
      val errors = if (skip) Nil else findScalaDoc(t.firstToken, fallback).
        map(_ => Nil).
        getOrElse(List(LineError(line, List(Missing))))

      // we don't descend any further
      errors

    case t: PatDefOrDcl  =>
      // val a = ...
      // var a = ...
      val (_, line) = lines.findLineAndIndex(t.valOrVarToken.offset).get
      val errors = if (skip) Nil else findScalaDoc(t.firstToken, fallback).
        map(_ => Nil).
        getOrElse(List(LineError(line, List(Missing))))
      // we don't descend any further
      errors

    case t: Any          =>
      // anything else, we descend (unless we stopped above)
      visit(t, localVisit(skip, fallback, lines))
  }

}

/**
 * Contains the ScalaDoc model with trivial parsers
 */
object ScalaDocChecker {
  val Missing = "Missing"
  val MalformedParams = "Malformed @params"
  val MalformedTypeParams = "Malformed @tparams"
  val MalformedReturn = "Malformed @return"

  /**
   * Companion for the ScalaDoc object that parses its text to pick up its elements
   */
  private object ScalaDoc {
    private val ParamRegex = "@param\\W+(\\w+)\\W+(.*)".r
    private val TypeParamRegex = "@tparam\\W+(\\w+)\\W+(.*)".r
    private val ReturnRegex = "@return\\W+(.*)".r

    /**
     * Take the ``raw`` and parse an instance of ``ScalaDoc``
     * @param raw the token containing the ScalaDoc
     * @return the parsed instance
     */
    def apply(raw: Token): ScalaDoc = {
      def paramsInRegex(r: Regex): List[ScalaDocParameter] = r.findAllIn(raw.rawText).matchData.map(m => ScalaDocParameter(m.group(1), m.group(2))).toList

      val params = paramsInRegex(ParamRegex)
      val typeParams = paramsInRegex(TypeParamRegex)
      val returns = ReturnRegex.findFirstMatchIn(raw.text).map(_.group(1))

      ScalaDoc(raw.rawText, params, typeParams, returns, None)
    }
  }

  /**
   * Models a parameter: either plain or type
   * @param name the parameter name
   * @param text the parameter text
   */
  private case class ScalaDocParameter(name: String, text: String)

  /**
   * Models the parsed ScalaDoc
   * @param text arbitrary text
   * @param params the parameters
   * @param typeParams the type parameters
   * @param returns the returns clause, if present
   * @param throws the throws clause, if present
   */
  private case class ScalaDoc(text: String, params: List[ScalaDocParameter], typeParams: List[ScalaDocParameter],
                      returns: Option[String], throws: Option[String])
}

