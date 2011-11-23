package org.segl.scalastyle.scalariform

import java.lang.reflect.Constructor;
import scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.Tokens._
import _root_.scalariform.lexer.Token
import _root_.scalariform.parser._
import org.segl.scalastyle.ScalariformChecker
import org.segl.scalastyle._

class EqualsHashCodeChecker extends ScalariformChecker {
  class Clazz[+T <: AstNode](val name: Option[String], val position: Option[Int], val subs: List[Clazz[_ <: AstNode]]) {
    def isHashCode = false
    def isEquals = false
    override def toString(): String = "name=" + name + " position=" + position + " subs=" + subs
  }

  case class TmplClazz(_name: Option[String], _position: Option[Int], _subs: List[Clazz[_ <: AstNode]]) extends Clazz[TmplDef](_name, _position, _subs)
  case class FunDefOrDclClazz(_name: Option[String], _position: Option[Int], _subs: List[Clazz[_ <: AstNode]]) extends Clazz[FunDefOrDcl](_name, _position, _subs) {
    override def isHashCode = Some("hashCode") == name
    override def isEquals = Some("equals") == name
  }

  def verify(file: String, ast: CompilationUnit): List[Message] = {
    val classes: List[Clazz[_ <: AstNode]] = visit(ast.immediateChildren(0))
    
    val it = for (
        t <- classes;
        f <- traverse(t); 
        if (matches(f))
    ) yield {
      StyleError(file, "equalsHashCode", position = f.position)
    }

    return it.toList
  }
  
  private def traverse(t: Clazz[AstNode]): List[Clazz[AstNode]] = {
    val l = t.subs.map(traverse(_)).flatten
	if (matches(t)) t :: l else l
  }
  
  private def matches(t: Clazz[AstNode]) = {
    val hc = t.subs.exists(_.isHashCode)
    val eq = t.subs.exists(_.isEquals)

    (hc && !eq) || (!hc && eq)
  }

  private def getParams(p: ParamClauses): List[Param] = {
    p.paramClausesAndNewlines.map(_._1).flatMap(pc => pc.firstParamOption :: pc.otherParams.map(p => Some(p._2))).flatten
  }
  
  private def method(t: FunDefOrDcl): Option[String] = {
    if (t.nameToken.getText == "equals") {
      var paramTypes = getParams(t.paramClauses).map(p => typename(p.paramTypeOpt.get._2))
      if (paramTypes.size == 1 && paramTypes(0) == "java.lang.Object") Some("equals") else None
    } else if (t.nameToken.getText == "hashCode") {
      var paramTypes = getParams(t.paramClauses).map(p => typename(p.paramTypeOpt.get._2))
      if (paramTypes.size == 0) Some("hashCode") else None
    } else {
      None
    }
  }
  
  private def typename(t: Type): String = t.tokens.map(_.getText).mkString

  private def visit(ast: Any): List[Clazz[_ <: AstNode]] = ast match {
    case t: TmplDef                => List(TmplClazz(Some(t.name.getText), Some(t.name.startIndex), visit(t.templateBodyOption)))
    case t: FunDefOrDcl            => List(FunDefOrDclClazz(method(t), Some(t.nameToken.startIndex), visit(t.localDef)))
    case a: AstNode                => visit(a.immediateChildren)
    case t: Token                  => List()
    case Some(x)                   => visit(x)
    case xs @ (_ :: _)             => xs flatMap { visit(_) }
    case Left(x)                   => visit(x)
    case Right(x)                  => visit(x)
    case (l, r)                    => visit(l) ::: visit(r)
    case (x, y, z)                 => visit(x) ::: visit(y) ::: visit(z)
    case true | false | Nil | None => List()
  }
}