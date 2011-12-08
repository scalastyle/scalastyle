package org.segl.scalastyle.scalariform

import scalariform.parser.CompilationUnit
import _root_.scalariform.lexer.Tokens._
import scalariform.lexer.Token
import org.segl.scalastyle.{Message, StyleError, ScalariformChecker}


class ParametersChecker extends ScalariformChecker {
  val DefaultMaxParamNumber = 5

  /**
   * Entry point of the ScalariformChecker
   */
  def verify(file: String, ast: CompilationUnit) = {
    buildList(ast.tokens, file)
  }

  /**
   * Count the number of parameters from the list of token received
   */
  def countParams(potentialArgumentList: List[Token]): Int = {
    potentialArgumentList.head.getText match {
      case "(" => {
        val endOfArgs = potentialArgumentList.takeWhile(_.getText != ")")
        endOfArgs.filter(_.getText == ",") match {
          case Nil => 0 // in case of ()
          case x => x.size + 1 // As we count the number of comma, the number of parameters is +1
        }
      }
      case "[" => {
        // This ia a parametrized function, remove the [...] to reach the "(" and call again this method
        val paramatrizedRemoved = potentialArgumentList.dropWhile(_.getType != RBRACKET).tail
        countParams(paramatrizedRemoved)
      }
      case _ => 0 // in case of function without parenthesis
    }
  }

  /**
   * Build the list of message based on the token received.
   * Recursive call until all the token are processed
   */
  def buildList(xs: List[Token], file: String): List[Message] =
    xs match {
      case Nil => List() // the list is now empty
      case ys =>
        val (error, rest) = getError(ys, file)
        error match {
          case None => buildList(rest, file)
          case _ => error.get :: buildList(rest, file)
        }
    }

  /**
   * From the list of Token search for a "def"
   * If a def is found then analyse if the number of parameters is respected.
   * return a tuple with a message (if number of parameters not respected) and the rest of the token to process.
   */
  def getError(tokens: List[Token], file: String): (Option[Message], List[Token]) = {
    val potentialArgumentList = tokens.dropWhile(_.tokenType != DEF).drop(2)
    potentialArgumentList match {
      case Nil => (None, List()) // No more "def" to process
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