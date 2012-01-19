package org.segl.scalastyle.scalariform;

import _root_.scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.Tokens._
import _root_.scalariform.lexer.Token
import _root_.scalariform.parser._
import org.segl.scalastyle.ScalariformChecker
import org.segl.scalastyle._

object VisitorHelper {
  class Clazz[+T <: AstNode]()

  protected[scalariform] def visit[T](ast: Any, visitfn: (Any) => List[T]): List[T] = ast match {
    case a: AstNode => visitfn(a.immediateChildren)
    case t: Token => List()
    case Some(x) => visitfn(x)
    case xs @ (_ :: _) => xs flatMap { visitfn(_) }
    case Left(x) => visitfn(x)
    case Right(x) => visitfn(x)
    case (l, r) => visitfn(l) ::: visitfn(r)
    case (x, y, z) => visitfn(x) ::: visitfn(y) ::: visitfn(z)
    case true | false | Nil | None => List()
  }
}

abstract class AbstractMethodChecker extends ScalariformChecker {
  import VisitorHelper._

  type ListType = List[BaseClazz[_ <: AstNode]]

  class BaseClazz[+T <: AstNode](val t: T, val position: Option[Int], val subs: ListType) extends Clazz[T] {
    def is(fn: T => Boolean) = false
  }

  case class TmplClazz(_t: TmplDef, _position: Option[Int], _subs: ListType) extends BaseClazz[TmplDef](_t, _position, _subs)
  case class FunDefOrDclClazz(_t: FunDefOrDcl, _position: Option[Int], _subs: ListType) extends BaseClazz[FunDefOrDcl](_t, _position, _subs) {
    override def is(fn: FunDefOrDcl => Boolean) = fn(this.t)
  }

  final def verify(ast: CompilationUnit): List[ScalastyleError] = {
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

  def matches(t: BaseClazz[AstNode]): Boolean

  protected def getParams(p: ParamClauses): List[Param] = {
    p.paramClausesAndNewlines.map(_._1).flatMap(pc => pc.firstParamOption :: pc.otherParams.map(p => Some(p._2))).flatten
  }

  protected def typename(t: Type): String = t.tokens.map(_.getText).mkString

  private def localvisit(ast: Any): ListType = ast match {
    case t: TmplDef => List(TmplClazz(t, Some(t.name.startIndex), localvisit(t.templateBodyOption)))
    case t: FunDefOrDcl => List(FunDefOrDclClazz(t, Some(t.nameToken.startIndex), localvisit(t.localDef)))
    case t: Any => visit(t, localvisit)
  }

  protected def getParamTypes(pc: ParamClauses) = getParams(pc).map(p => typename(p.paramTypeOpt.get._2))

  def matchFunDefOrDcl(t: BaseClazz[AstNode], fn: FunDefOrDcl => Boolean) = t match { case f: FunDefOrDclClazz => fn(f.t); case _ => false }

  protected def methodMatch(name: String, paramTypesMatch: List[String] => Boolean)(t: FunDefOrDcl) =
    t.nameToken.getText == name && paramTypesMatch(getParamTypes(t.paramClauses))

  protected def singleParameter(fn: String => Boolean)(params: List[String]) = params.size == 1 && fn(params(0))
  protected def noParameter()(params: List[String]) = params.size == 0
  protected def isEqualsObject(t: FunDefOrDcl): Boolean = methodMatch("equals", singleParameter(isObject) _)(t)
}

