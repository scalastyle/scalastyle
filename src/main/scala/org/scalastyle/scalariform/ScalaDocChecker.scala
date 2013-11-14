package org.scalastyle.scalariform

import _root_.scalariform.lexer.{HiddenTokens, Token}
import _root_.scalariform.parser._
import _root_.scalariform.parser.FunDefOrDcl
import _root_.scalariform.parser.TmplDef
import org.scalastyle._
import org.scalastyle.scalariform.VisitorHelper._
import org.scalastyle.CombinedAst
import scala.util.matching.Regex

class ScalaDocChecker extends CombinedChecker {
  protected val errorKey: String = "scaladoc"
  private val Missing = "missing"
  private val MalformedParams = "malformedParams"
  private val MalformedTypeParams = "malformedTypeParams"
  private val MalformedReturn = "malformedReturn"

  private val skipPrivate = true
  private val skipQualifiedPrivate = false
  private val skipProtected = false
  private val skipQualifiedProtected = false

  case class DocumentedMemberClazz(t: DefOrDcl, position: Option[Int], subs: List[DocumentedMemberClazz]) extends Clazz[DefOrDcl]()

  def verify(ast: CombinedAst): List[ScalastyleError] = {
    visit0(false, HiddenTokens(Nil), ast.lines)(ast.compilationUnit.immediateChildren(0))
  }

  private def findScalaDoc(token: Token, fallback: HiddenTokens): Option[ScalaDoc] = {
    def toScalaDoc(ht: HiddenTokens): Option[ScalaDoc] = ht.rawTokens.find(_.isScalaDocComment).map(ScalaDoc.apply)
    
    toScalaDoc(token.associatedWhitespaceAndComments).orElse(toScalaDoc(fallback))
  }

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

  private def returnErrors(line: Int, returnTypeOpt: Option[(Token, Type)])(scalaDoc: ScalaDoc): List[ScalastyleError] = {
    val needsReturn = returnTypeOpt.map {
      case (_, tpe) => tpe.firstToken.text != "Unit"
    }.getOrElse(false)

    if (needsReturn && !scalaDoc.returns.isDefined) {
      List(LineError(line, List(MalformedReturn)))
    } else {
      Nil
    }
  }

  private def visit0(skip: Boolean, fallback: HiddenTokens, lines: Lines)(ast: Any): List[ScalastyleError] = ast match {
    case t: FullDefOrDcl      =>
      val skip = t.modifiers match {
        case AccessModifier(pop, Some(_))::_ =>
          if (pop.text == "private") skipQualifiedPrivate
          else                       skipQualifiedProtected
        case AccessModifier(pop, None)::_ =>
          if (pop.text == "private") skipPrivate
          else                       skipProtected
        case _                       =>
          false
      }
      
      val scalaDoc = t.modifiers match {
        case AccessModifier(pop, _)::_ => pop.associatedWhitespaceAndComments
        case _                         => HiddenTokens(Nil)
      }

      visit(t, visit0(skip, scalaDoc, lines))
    case t: TmplDef      =>
      val (_, line) = lines.findLineAndIndex(t.firstToken.offset).get
      val errors = if (skip) Nil else findScalaDoc(t.firstToken, fallback).
        map { scalaDoc =>
          paramErrors(line, t.paramClausesOpt)(scalaDoc) ++
          tparamErrors(line, t.typeParamClauseOpt)(scalaDoc)
        }.getOrElse(List(LineError(line, List(Missing))))

      errors ++ visit(t, visit0(skip, fallback, lines))
    case t: FunDefOrDcl  =>
      val (_, line) = lines.findLineAndIndex(t.firstToken.offset).get

      val errors = if (skip) Nil else findScalaDoc(t.firstToken, fallback).
        map { scalaDoc =>
          paramErrors(line, Some(t.paramClauses))(scalaDoc) ++
          tparamErrors(line, t.typeParamClauseOpt)(scalaDoc) ++
          returnErrors(line, t.returnTypeOpt)(scalaDoc)
        }.
        getOrElse(List(LineError(line, List(Missing))))

      errors ++ visit(t, visit0(skip, fallback, lines))
    case t: Any          =>
      visit(t, visit0(skip, fallback, lines))
  }

}

object ScalaDoc {
  private val ParamRegex = "@param\\W+(\\w+)\\W+(.*)".r
  private val TypeParamRegex = "@tparam\\W+(\\w+)\\W+(.*)".r
  private val ReturnRegex = "@return\\W+(.*)".r

  def apply(raw: Token): ScalaDoc = {
    def paramsInRegex(r: Regex): List[ScalaDocParameter] = r.findAllIn(raw.rawText).matchData.map(m => ScalaDocParameter(m.group(1), m.group(2))).toList

    val params = paramsInRegex(ParamRegex)
    val typeParams = paramsInRegex(TypeParamRegex)
    val returns = ReturnRegex.findFirstMatchIn(raw.text).map(_.group(1))

    ScalaDoc(raw.rawText, params, typeParams, returns, None)
  }
}

case class ScalaDocParameter(name: String, text: String)

case class ScalaDoc(text: String, params: List[ScalaDocParameter], typeParams: List[ScalaDocParameter],
                    returns: Option[String], throws: Option[String])