// Copyright (C) 2011-2012 the original author or authors.
// See the LICENCE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package org.scalastyle.scalariform

import java.util.regex.Pattern

import org.scalastyle.PositionError
import org.scalastyle.ScalariformChecker
import org.scalastyle.ScalastyleError
import org.scalastyle.scalariform.VisitorHelper.visit

import scala.Array.canBuildFrom
import scala.collection.mutable.ListBuffer
import _root_.scalariform.lexer.MultiLineComment
import _root_.scalariform.lexer.Token
import _root_.scalariform.lexer.Whitespace
import _root_.scalariform.parser.AstNode
import _root_.scalariform.parser.BlockImportExpr
import _root_.scalariform.parser.CompilationUnit
import _root_.scalariform.parser.Expr
import _root_.scalariform.parser.ExprElement
import _root_.scalariform.parser.GeneralTokens
import _root_.scalariform.parser.ImportClause
import _root_.scalariform.parser.ImportSelectors
import scala.util.matching.Regex

// scalastyle:off multiple.string.literals

abstract class AbstractImportChecker extends ScalariformChecker {
  case class ImportClauseVisit(t: ImportClause, importExpr: List[ImportClauseVisit], otherImportExprs: List[ImportClauseVisit])

  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    init()

    val it = for {
      t <- localvisit(ast.immediateChildren)
      f <- traverse(t)
    } yield {
      PositionError(t.t.firstToken.offset)
    }

    it
  }

  protected def init(): Unit = {}

  private[this] def traverse(t: ImportClauseVisit): List[ImportClauseVisit] = {
    val l = t.importExpr.flatMap(traverse) ::: t.otherImportExprs.flatMap(traverse)
    if (matches(t)) t :: l else l
  }

  private[this] def imports(tokens: List[Token]): String = {
    tokens.foldLeft("")((a, b) => a + b.text)
  }

  private[this] def imports(t: BlockImportExpr): List[String] = {
    val is = t.importSelectors

    val firsts = is.firstImportSelector.firstToken.text ::
            is.otherImportSelectors.map(_._2).map(is => is.firstToken.text)
    firsts.map(f => imports(t.prefixExpr.tokens) + f)
  }

  protected final def imports(t: ImportClauseVisit): List[String] = {
    t.t.importExpr match {
      case t: BlockImportExpr => imports(t)
      case _ => List(imports(t.t.importExpr.tokens))
    }
  }

  def matches(t: ImportClauseVisit): Boolean

  private[this] def localvisit(ast: Any): List[ImportClauseVisit] = ast match {
    case t: ImportClause => List(ImportClauseVisit(t, localvisit(t.importExpr), localvisit(t.otherImportExprs)))
    case t: Any => visit(t, localvisit)
  }
}

class IllegalImportsChecker extends AbstractImportChecker {
  val errorKey = "illegal.imports"

  val DefaultIllegalImports = "sun._"
  var illegalImportsList: List[String] = _
  var exemptImportsList: List[String] = _

  // sun._ => sun\.
  // sun.com.foobar => sun\.com\.foobar
  private def toMatchList(s: String) = {
    s.trim().split(" *, *").map(s => s.replaceAll("_$", "")).toList
  }

  override protected def init() = {
    illegalImportsList = toMatchList(getString("illegalImports", DefaultIllegalImports))
    exemptImportsList = toMatchList(getString("exemptImports", ""))
  }

  def matches(t: ImportClauseVisit): Boolean = {
    val list = imports(t)
    val revisedList = list diff exemptImportsList
    illegalImportsList.exists(ill => revisedList.exists(_.startsWith(ill)))
  }
}

class UnderscoreImportChecker extends AbstractImportChecker {
  private val DefaultIgnoreRegex = "^$"
  val errorKey = "underscore.import"

  private var ignoreRegex: Regex = _

  override protected def init(): Unit = {
    ignoreRegex = getString("ignoreRegex", DefaultIgnoreRegex).r
  }

  def matches(t: ImportClauseVisit): Boolean = imports(t)
      .filterNot((importStatement) => ignoreRegex.findFirstIn(importStatement).isDefined)
      .exists(_.endsWith("._"))
}

class ImportGroupingChecker extends ScalariformChecker {
  val errorKey = "import.grouping"

  def verify(ast: CompilationUnit): List[ScalastyleError] = {
    val it = VisitorHelper.getAll[ImportClause](ast.immediateChildren)

    if (it.isEmpty) {
      List()
    } else {
      val importTokens = it.flatMap(_.tokens)
      val (min, max) = (importTokens.head.offset, importTokens.last.offset)

      val s = ast.tokens.find(t => t.offset >= min && t.offset <= max && !t.isNewline && !(t.text == ";") && !importTokens.contains(t))

      s match {
        case Some(x) => it.dropWhile(ic => ic.firstToken.offset <= x.offset).map(ic => PositionError(ic.firstToken.offset))
        case None => List()
      }
    }
  }
}

/**
 * Style checker that enforces import ordering. The following configuration parameters are
 * available:
 *
 *  - groups: a comma-separated list of group names to consider.
 *  - maxBlankLines: maximum number of blank lines to allow between groups. The default is "1".
 *                   A value less than 1 disables the blank line limit.
 *  - group.[groupName]: a regular expression that matches imports that should be in the given
 *                       group.
 *  - lexicographic: if true, imports are ordered lexicographically (classes, wildcards, then
 *                   packages; case-sensitive ordering within); if false, apply the original
 *                   case-insensitive ordering (with wildcards coming first, before classes).
 *  - intellij: if true, imports are ordered in Intellij-style.
 *
 * For example, to check that "java" and "javax" imports are in a separate group at the top of the
 * import list, you'd use this config:
 *
 *  <parameter name="groups">java,others</parameter>
 *  <parameter name="group.java">javax?\..+</parameter>
 *  <parameter name="group.other">.+</parameter>
 *
 * Other non-configurable rules:
 * - Within each group, import clauses are ordered alphabetically if 'lexicographic' is
 *   specified; else puts wildcards, then classes and packages, with case-insensitive sort.
 * - In multi-import statements, entries are ordered alphabetically, with method / packages
 *   (assumed to be any string starting with a lower case letter) coming before classes.
 *
 * Currently, this checker only looks at the top-level list of imports.
 *
 * Setting both lexicographic and intellij to true has unspecified behavior.
 */
class ImportOrderChecker extends ScalariformChecker {
  val errorKey: String = "import.ordering"

  private var groups: Seq[(String, Pattern)] = _
  private var maxBlankLines: Int = _
  private var lexicographic: Boolean = _
  private var intellij: Boolean = _

  private var ast: AstNode = _
  private var lastImport: Option[AstNode] = None

  private var currentGroup = 0
  private var lastImportInGroup: Option[String] = None

  override def setParameters(parameters: Map[String, String]): Unit = {
    // Note that any exceptions thrown here are swallowed by CheckerUtils and ignored...
    require(parameters.contains("groups"))
    groups = parameters("groups").split(",").map { name =>
      name -> Pattern.compile(parameters(s"group.${name}"))
    }
    maxBlankLines = parameters.getOrElse("maxBlankLines", "1").toInt
    lexicographic = parameters.get("lexicographic").map(_.toBoolean).getOrElse(false)
    intellij = parameters.get("intellij").map(_.toBoolean).getOrElse(false)
  }

  override def verify(ast: CompilationUnit): List[ScalastyleError] = {
    this.ast = ast

    val CompilationUnit(statements, _) = ast
    statements.immediateChildren.flatMap { n =>
      val result = n match {
        case ImportClause(_, BlockImportExpr(prefix, selectors), _) =>
          val text = exprToText(prefix.contents, Some(selectors))
          checkImport(text, n.firstToken.offset) ++ checkSelectors(selectors)

        case ImportClause(_, Expr(contents), _) =>
          val text = exprToText(contents, None)
          checkImport(text, n.firstToken.offset)

        case _ =>
          Nil
      }
      lastImport = Some(n)
      result
    }
  }

  private def exprToText(contents: List[ExprElement], selectors: Option[ImportSelectors]): String = {
    contents.flatMap {
      case GeneralTokens(toks) => toks.map(_.text)
      case n: Any => throw new IllegalStateException(s"FIXME: unexpected expr child node $n")
    }.mkString("") ++ selectors.flatMap { selectors =>
      if (intellij) {
        // Use the import selectors like Intellij Idea does.
        val ImportSelectors(_, first, others, _) = selectors
        val tokens = first.contents.head.tokens.head.text :: others.map(_._2.contents.head.tokens.head.text)
        Some("{" + tokens.mkString(", ") + "}")
      } else {
        None
      }
    }.mkString("")
  }

  /**
   * Check that the given import belongs to the current group and is ordered correctly within it.
   */
  private def checkImport(str: String, offset: Int): Seq[ScalastyleError] = {
    val errors = new ListBuffer[ScalastyleError]()

    if (!groups(currentGroup)._2.matcher(str).matches()) {
      // If a statement doesn't match the current group, there are two options:
      // - It belongs to a previous group, in which case an error is flagged.
      // - It belongs to a following group, in which case the group index moves forward.
      for (i <- 0 until currentGroup) {
        if (groups(i)._2.matcher(str).matches()) {
          return Seq(newError(offset, "wrongGroup", str, groups(i)._1, groups(currentGroup)._1))
        }
      }

      var nextGroup = currentGroup + 1
      while (nextGroup < groups.size && !groups(nextGroup)._2.matcher(str).matches()) {
        nextGroup += 1
      }

      if (nextGroup == groups.size) {
        throw new IllegalStateException(s"FIXME: import statement does not match any group: $str")
      }

      errors ++= checkGroupSeparation(currentGroup, nextGroup, offset)
      currentGroup = nextGroup
      lastImportInGroup = None
    } else {
      // If the statement is in the same group, make sure there is no empty line between it and
      // the previous import.
      errors ++= checkNoSeparator(offset)
    }

    // Ensure import is in alphabetical order.
    if (lastImportInGroup.isDefined && compareImports(lastImportInGroup.get, str) > 0) {
      errors += newError(offset, "wrongOrderInGroup", str, lastImportInGroup.get)
    }

    lastImportInGroup = Some(str)
    errors
  }

  /**
   * Check that the imports inside a multi-import block are ordered.
   */
  private def checkSelectors(selectors: ImportSelectors): Seq[ScalastyleError] = {
    val ImportSelectors(_, first, others, _) = selectors

    val errors = new ListBuffer[ScalastyleError]()
    val names = Seq(first.contents.head.tokens.head.text) ++
      others.map( _._2.contents.head.tokens.head.text)

    if (names.size > 1) {
      names.sliding(2).foreach { case Seq(left, right) =>
        if (compareNames(left, right, isImport = false) > 0) {
          errors += newError(selectors.firstToken.offset, "wrongOrderInSelector", right, left)
        }
      }
    }

    errors
  }

  /**
   * When the current import group changes, checks that there is a single empty line between
   * the last import statement in the previous group and the first statement in the new one.
   */
  private def checkGroupSeparation(
      lastGroup: Int,
      nextGroup: Int,
      nextGroupOffset: Int): Option[ScalastyleError] = {
    if (lastGroup != nextGroup && lastImport.isDefined) {
      val start = lastImport.get.lastToken.offset + lastImport.get.lastToken.length
      val separatorLines = countNewLines(start, nextGroupOffset) - 1
      val last = groups(lastGroup)._1
      val current = groups(nextGroup)._1
      if (separatorLines == 0) {
        return Some(newError(nextGroupOffset, "missingEmptyLine", last, current))
      } else if (maxBlankLines > 0 && separatorLines > maxBlankLines) {
        return Some(newError(nextGroupOffset, "tooManyEmptyLines", maxBlankLines, last, current))
      }
    }

    None
  }

  /**
   * Check that there are no empty lines between imports in the same group.
   */
  private def checkNoSeparator(offset: Int): Option[ScalastyleError] = {
    if (lastImportInGroup.isDefined) {
      val start = lastImport.get.lastToken.offset + lastImport.get.lastToken.length
      if (countNewLines(start, offset) != 1) {
        return Some(newError(offset, "noEmptyLine"))
      }
    }
    None
  }

  /**
   * Counts the number of new lines between the given offsets, adjusted for comments.
   */
  private def countNewLines(start: Int, end: Int): Int = {
    var count = 0
    ast.tokens.filter { t => t.offset >= start && t.offset < end }.foreach { t =>
      val commentsToken = t.associatedWhitespaceAndComments
      if (commentsToken != null) { // scalastyle:ignore null
        var ignoreNext = false
        commentsToken.tokens.foreach {
          case c: MultiLineComment =>
            // Do not count a new line after a multi-line comment.
            ignoreNext = true
          case w: Whitespace =>
            if (!ignoreNext) {
              // Assumes "\n" only used for new lines.
              count += w.text.count(_ == '\n')
            }
            ignoreNext = true
          case _ =>
            // Nothing to do.
        }
      }
    }
    count
  }

  /**
   * Compares two import statements, comparing each component of the import separately.
   *
   * The import statements can end with a dangling `.`, meaning they're the start of a
   * multi-import block.
   */
  private[scalariform] def compareImports(imp1: String, imp2: String): Int = {
    val imp1Components = imp1.split("[.]")
    val imp2Components = imp2.split("[.]")
    val max = math.min(imp1Components.size, imp2Components.size)
    for (i <- 0 until max) {
      val comp1 = imp1Components(i)
      val comp2 = imp2Components(i)
      val result = compareNames(comp1, comp2, isImport = true)
      if (result != 0) {
        return result
      }
    }

    // At this point, there is still a special case: where one import is a multi-import block
    // (and, thus, has no extra components) and another is a wildcard; the wildcard should come
    // first.
    val diff = imp1Components.size - imp2Components.size
    if (diff == -1 && imp1.endsWith(".") && imp2Components.last == "_") {
      1
    } else if (diff == 1 && imp2.endsWith(".") && imp1Components.last == "_") {
      -1
    } else {
      diff
    }
  }

  /**
   * Compares two strings that represent a single imported artifact; this considers lower-case
   * names as being "lower" than upper case ones.
   *
   * @param name1 First name.
   * @param name2 Second name.
   * @param isImport If true, orders names according to the import statement rules:
   *                 "_" should come before other names, and capital letters should come
   *                 before lower case ones. Otherwise, do the opposite, which are the ordering
   *                 rules for names within a selector.
   */
  private[scalariform] def compareNames(name1: String, name2: String, isImport: Boolean): Int = {
    if (lexicographic && isImport) {
      name1.compareTo(name2)
    } else if (name1 != "_") {
      if (name2 == "_") {
        -1 * compareNames(name2, name1, isImport)
      } else {
        val isName1UpperCase = Character.isUpperCase(name1.codePointAt(0))
        val isName2UpperCase = Character.isUpperCase(name2.codePointAt(0))

        if (isName1UpperCase == isName2UpperCase) {
          name1.compareToIgnoreCase(name2)
        } else {
          // Classes come before subpackages in import statements, after in selectors.
          val order = if (isImport) -1 else 1
          if (isName1UpperCase) order else -order
        }
      }
    } else {
      if (isImport) -1 else 1
    }
  }

  private def newError(offset: Int, errorKey: String, args: Any*): ScalastyleError = {
    PositionError(offset, args.map(_.toString).toList, Some(this.errorKey + "." + errorKey))
  }

}
