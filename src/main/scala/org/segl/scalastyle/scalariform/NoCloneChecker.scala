package org.segl.scalastyle.scalariform

import java.lang.reflect.Constructor;
import scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.Tokens._
import _root_.scalariform.lexer.Token
import _root_.scalariform.parser._
import org.segl.scalastyle.ScalariformChecker
import org.segl.scalastyle._

class NoCloneChecker extends AbstractMethodChecker {
  val errorKey = "no.clone"

  def matches(t: BaseClazz[AstNode]): Boolean = {
    t.subs.exists(matchFunDefOrDcl(_, isClone))
  }

  private def isClone(t: FunDefOrDcl): Boolean = methodMatch("clone", noParameter() _)(t)
}