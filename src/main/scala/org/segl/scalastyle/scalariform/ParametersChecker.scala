package org.segl.scalastyle.scalariform

import scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.Tokens._
import scalariform.lexer.Token
import org.segl.scalastyle.{Message, StyleError, ScalariformChecker}


class ParametersChecker extends ScalariformChecker {
  val DefaultMaxParamNumber = 5


  def countParams(potentialArgumentList: List[Token]): Int = {
    potentialArgumentList.head.getText match {
      case "(" => {
        val endOfArgs = potentialArgumentList.takeWhile(_.getText != ")")
        endOfArgs.filter(_.getText == ",") match {
          case Nil => 0 // in case of ()
          case x => x.size + 1
        }
      }
      case "[" => {
        val paramatrizedRemoved = potentialArgumentList.dropWhile(_.getType != RBRACKET).tail
        countParams(paramatrizedRemoved)
      }
      case _ => 0 // in case of no parenthesis
    }
  }


  def verify(file: String, ast: CompilationUnit) = {
    buildList(ast.tokens, file)
  }

  final def buildList(xs: List[Token], file: String): List[Message] =
    xs match {
      case Nil => List()
      case ys =>
        val (error, rest) = getError(ys, file)
        error match {
          case None => buildList(rest, file)
          case _ => error.get :: buildList(rest, file)
        }
    }

  def getError(tokens: List[Token], file: String): (Option[Message], List[Token]) = {
    val potentialArgumentList = tokens.dropWhile(_.tokenType != DEF).drop(2)
    potentialArgumentList match {
      case Nil => (None, List())
      case _ => {
        if (countParams(potentialArgumentList) > getInt("maxParamNumber", DefaultMaxParamNumber)) {
          (Option(StyleError(file, "nb.of.parameter", position = Some(potentialArgumentList.head.startIndex))), potentialArgumentList)
        } else {
          (None, potentialArgumentList)
        }
      }
    }
  }
}