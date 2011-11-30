package org.segl.scalastyle.scalariform

import java.lang.reflect.Constructor;
import scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.Tokens._
import _root_.scalariform.lexer.Token
import _root_.scalariform.parser._
import org.segl.scalastyle.ScalariformChecker
import org.segl.scalastyle._

class ParameterNumberChecker extends ScalariformChecker {
  import VisitorHelper._
  val DefaultMaximumParameters = 8

  case class FunDefOrDclClazz(paramClauses: ParamClauses, position: Option[Int], subs: List[FunDefOrDclClazz]) extends Clazz[FunDefOrDcl]()

  def verify(file: String, ast: CompilationUnit): List[Message] = {
    val maximumParameters = getInt("maxParameters", DefaultMaximumParameters)

    val it = for (
        t <- localvisit(ast.immediateChildren(0));
        f <- traverse(t);
        if (matches(f, maximumParameters))
    ) yield {
      StyleError(file, "parameterNumber", position = t.position)
    }

    it.toList
  }

  private def traverse(t: FunDefOrDclClazz): List[FunDefOrDclClazz] = {
    t :: t.subs.map(traverse(_)).flatten
  }

  private def matches(t: FunDefOrDclClazz, maximumParameters: Int) = getParams(t.paramClauses).size > maximumParameters

  private def getParams(p: ParamClauses): List[Param] = {
    p.paramClausesAndNewlines.map(_._1).flatMap(pc => pc.firstParamOption :: pc.otherParams.map(p => Some(p._2))).flatten
  }

  private def typename(t: Type): String = t.tokens.map(_.getText).mkString

  private def localvisit(ast: Any): List[FunDefOrDclClazz] = ast match {
    case t: FunDefOrDcl => List(FunDefOrDclClazz(t.paramClauses, Some(t.nameToken.startIndex), localvisit(t.localDef)))
    case t: Any         => visit(t, localvisit)
  }
}