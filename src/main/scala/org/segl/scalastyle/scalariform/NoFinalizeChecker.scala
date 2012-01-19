package org.segl.scalastyle.scalariform

import java.lang.reflect.Constructor;
import scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.Tokens._
import _root_.scalariform.lexer.Token
import _root_.scalariform.parser._
import org.segl.scalastyle.ScalariformChecker
import org.segl.scalastyle._

class NoFinalizeChecker extends AbstractMethodChecker {
  val errorKey = "no.finalize"

  def matches(t: BaseClazz[AstNode]): Boolean = {
    t.subs.exists(matchFunDefOrDcl(_, isFinalize))
  }

  private def isFinalize(t: FunDefOrDcl): Boolean = methodMatch("finalize", noParameter() _)(t)
}