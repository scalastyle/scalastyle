package org.segl.scalastyle.scalariform

import java.lang.reflect.Constructor;
import scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.Tokens._
import _root_.scalariform.lexer.Token
import _root_.scalariform.parser._
import org.segl.scalastyle.ScalariformChecker
import org.segl.scalastyle._

class CovariantEqualsChecker extends AbstractMethodChecker {
  import VisitorHelper._
  val errorKey = "covariant.equals"

  def matches(t: BaseClazz[AstNode]): Boolean = {
    val equalsObject = t.subs.exists(matchFunDefOrDcl(_, isEqualsObject))
    val equalsOther = t.subs.exists(matchFunDefOrDcl(_, isEqualsOther))

    !equalsObject && equalsOther
  }

  private def isEqualsOther(t: FunDefOrDcl): Boolean = methodMatch("equals", singleParameter(isNotObject) _)(t)
}