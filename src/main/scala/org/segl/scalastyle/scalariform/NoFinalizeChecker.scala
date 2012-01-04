package org.segl.scalastyle.scalariform

import java.lang.reflect.Constructor;
import scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.Tokens._
import _root_.scalariform.lexer.Token
import _root_.scalariform.parser._
import org.segl.scalastyle.ScalariformChecker
import org.segl.scalastyle._

class NoFinalizeChecker extends ScalariformChecker {
  import VisitorHelper._
  val errorKey = "no.finalize"

  type ListType = List[BaseClazz[_ <: AstNode]]

  class BaseClazz[+T <: AstNode](val name: Option[String], val position: Option[Int], val subs: ListType) extends Clazz[T] {
    def isFinalize = false
    override def toString(): String = "name=" + name + " position=" + position + " subs=" + subs
  }

  case class TmplClazz(_name: Option[String], _position: Option[Int], _subs: ListType) extends BaseClazz[TmplDef](_name, _position, _subs)
  case class FunDefOrDclClazz(_name: Option[String], _position: Option[Int], _subs: ListType) extends BaseClazz[FunDefOrDcl](_name, _position, _subs) {
    override def isFinalize = Some("finalize") == name
  }

  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val it = for (
        t <- localvisit(ast.immediateChildren(0));
        f <- traverse(t);
        if (matches(f))
    ) yield {
      PositionError(f.position.get)
    }

    it.toList
  }

  private def traverse(t: BaseClazz[AstNode]): ListType = {
    val l = t.subs.map(traverse(_)).flatten
    if (matches(t)) t :: l else l
  }

  private def matches(t: BaseClazz[AstNode]) = {
    t.subs.exists(_.isFinalize)
  }

  private def getParams(p: ParamClauses): List[Param] = {
    p.paramClausesAndNewlines.map(_._1).flatMap(pc => pc.firstParamOption :: pc.otherParams.map(p => Some(p._2))).flatten
  }

  private def method(t: FunDefOrDcl): Option[String] = {
    if (t.nameToken.getText == "finalize") {
      var paramTypes = getParams(t.paramClauses).map(p => typename(p.paramTypeOpt.get._2))
      if (paramTypes.size == 0) {
        Some("finalize")
      } else {
        None
      }
    } else {
      None
    }
  }

  private def typename(t: Type): String = t.tokens.map(_.getText).mkString

  private def localvisit(ast: Any): ListType = ast match {
    case t: TmplDef     => List(TmplClazz(Some(t.name.getText), Some(t.name.startIndex), localvisit(t.templateBodyOption)))
    case t: FunDefOrDcl => List(FunDefOrDclClazz(method(t), Some(t.nameToken.startIndex), localvisit(t.localDef)))
    case t: Any         => visit(t, localvisit)
  }
}