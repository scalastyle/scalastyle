package org.segl.scalastyle.scalariform

import scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.Tokens._
import _root_.scalariform.lexer.Token
import _root_.scalariform.parser._
import org.segl.scalastyle.ScalariformChecker
import org.segl.scalastyle._

class EqualsHashCodeChecker extends AbstractMethodChecker {
  val errorKey = "equals.hash.code"

  def matches(t: BaseClazz[AstNode]): Boolean = {
    val hc = t.subs.exists(matchFunDefOrDcl(_, isHashCode))
    val eq = t.subs.exists(matchFunDefOrDcl(_, isEqualsObject))

    (hc && !eq) || (!hc && eq)
  }

  private def isHashCode(t: FunDefOrDcl): Boolean = methodMatch("hashCode", noParameter() _)(t)
}
