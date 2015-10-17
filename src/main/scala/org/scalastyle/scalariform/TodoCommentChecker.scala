package org.scalastyle.scalariform

import java.util.regex.Pattern

import org.scalastyle.CombinedAst
import org.scalastyle.CombinedChecker
import org.scalastyle.PositionError
import org.scalastyle.ScalastyleError

import scalariform.lexer.Tokens

/**
 * comment check for line comment style TODO or FIXME
 */
class TodoCommentChecker extends CombinedChecker {
  val errorKey = "todo.comment"
  val defaultWords = "TODO|FIXME"

  def verify(ast: CombinedAst): List[ScalastyleError] = {
    val words = getString("words", defaultWords)
    val split = words.split("\\|").map(Pattern.quote).mkString("|")
    val regex = ("""(?i)(//|/\*|/\*\*|\*)\s?(""" + split + """)(:?)\s+""").r

    for {
      t <- ast.compilationUnit.tokens
      at <- t.associatedWhitespaceAndComments
      if Tokens.COMMENTS.contains(at.token.tokenType)
      if at.text.split("\n").exists(s => regex.findFirstIn(s).isDefined)
    } yield PositionError(at.token.offset, List(words))
  }
}
