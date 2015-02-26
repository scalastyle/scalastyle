package org.scalastyle.file

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalastyle.file.TodoCommentChecker._

class TodoCommentCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "todo.comment"
  val classUnderTest = classOf[TodoCommentChecker]

  @Test def testSourceWithoutTodoComments(): Unit = {
    val source =
      """
        |package foobar
        |
        |/**
        | * Todoy is a misspelling of today
        | */
        |class foobar {
        |  var myField = "one"
        |
        |  // This inline comment contains the word todo
        |  def doSomething(): Unit = {}
        |
        |  // Todoy is a misspelling of today
        |  def doSomethingElse() = ???
        |
        |  // To do this is...
        |  def doNothing() = ???
        |
        |  /*
        |   * Todoy, I'm very bad at spelling today
        |   */
        |   def stillDoNothing() = ???
        |}
      """.stripMargin

    assertErrors(List(), source)
  }

  @Test def testSourceWithTodoComments(): Unit = {
    val source =
      """
        |package foobar
        |
        |class foobar {
        |  // TODO make this a val
        |  var myField = "one"
        |
        |  def doSomething(): Unit = {
        |    // Todo add some logic
        |  }
        |
        |  // ToDo implement this
        |  def doSomethingElse() = ???
        |
        |  def doNothing() = ??? // todo implement this too
        |
        |  def stillDoNothing() = ??? // TODO: implement this too
        |
        |  def nothing() = ??? /* TODO */
        |
        |  /**
        |   * TODO Add some more code
        |   * todo: refactor a little
        |   */
        |
        |   /** TODO reformat */
        |   /* TODO removed all these todos */
        |}
      """.stripMargin

    assertErrors(
      List(
        columnError(5, 2, List(TodoRegex)),
        columnError(9, 4, List(TodoRegex)),
        columnError(12, 2, List(TodoRegex)),
        columnError(15, 24, List(TodoRegex)),
        columnError(17, 29, List(TodoRegex)),
        columnError(19, 22, List(TodoRegex)),
        columnError(22, 3, List(TodoRegex)),
        columnError(23, 3, List(TodoRegex)),
        columnError(26, 3, List(TodoRegex)),
        columnError(27, 3, List(TodoRegex))
      ),
      source)
  }
}
