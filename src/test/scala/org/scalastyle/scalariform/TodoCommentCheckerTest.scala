package org.scalastyle.scalariform

import org.junit.Test
import org.scalastyle.file.CheckerTestHelper
import org.scalatest.junit.AssertionsForJUnit

class TodoCommentCheckerTest extends AssertionsForJUnit with CheckerTestHelper {
  val key = "todo.comment"
  val text = "TODO|FIXME"
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
        |
        |   val x = "// TODO what about in a string?"
        |}
      """.stripMargin

    assertErrors(
      List(
        columnError(5, 2, List(text)),
        columnError(9, 4, List(text)),
        columnError(12, 2, List(text)),
        columnError(15, 24, List(text)),
        columnError(17, 29, List(text)),
        columnError(19, 22, List(text)),
        columnError(21, 2, List(text)),
        columnError(26, 3, List(text)),
        columnError(27, 3, List(text))
      ),
      source)
  }


  @Test def testSourceWithoutFixmeComments(): Unit = {
    val source =
      """
        |package foobar
        |
        |/**
        | * Fix mechanism is a misspelling of fix mechanism
        | */
        |class foobar {
        |  var myField = "one"
        |
        |  // This inline comment contains the word fixme
        |  def doSomething(): Unit = {}
        |
        |  // Fixmechanism is a misspelling of fix mechanism
        |  def doSomethingElse() = ???
        |
        |  // Fix mechanism...
        |  def doNothing() = ???
        |
        |  /*
        |   * Fix mechanism, I'm very bad at spelling fix mechanism
        |   */
        |   def stillDoNothing() = ???
        |}
      """.stripMargin

    assertErrors(List(), source)
  }

  @Test def testSourceWithFixmeComments(): Unit = {
    val source =
      """
        |package foobar
        |
        |class foobar {
        |  // FIXME make this a val
        |  var myField = "one"
        |
        |  def doSomething(): Unit = {
        |    // Fixme add some logic
        |  }
        |
        |  // FixMe implement this
        |  def doSomethingElse() = ???
        |
        |  def doNothing() = ??? // fixme this too
        |
        |  def stillDoNothing() = ??? // FIXME: this too
        |
        |  def nothing() = ??? /* FIXME */
        |
        |  /**
        |   * FIXME Add some more code
        |   * fixme: refactor a little
        |   */
        |
        |   /** FIXME reformat */
        |   /* FIXME removed all these fixmes */
        |}
      """.stripMargin

    assertErrors(
      List(
        columnError(5, 2, List(text)),
        columnError(9, 4, List(text)),
        columnError(12, 2, List(text)),
        columnError(15, 24, List(text)),
        columnError(17, 29, List(text)),
        columnError(19, 22, List(text)),
        columnError(21, 2, List(text)),
        columnError(26, 3, List(text)),
        columnError(27, 3, List(text))
      ),
      source)
  }

  @Test def testSourceWithAlternativeComments(): Unit = {
    val source =
      """
        |package foobar
        |
        |class foobar {
        |  // AFAIRE make this a val
        |  var myField = "one"
        |
        |  def doSomething(): Unit = {
        |    // afaire add some logic
        |  }
        |
        |  // AFaire implement this
        |  def doSomethingElse() = ???
        |
        |  def doNothing() = ??? // TODO this should be ignore
        |
        |   /** FIXME reformat */
        |}
      """.stripMargin

    val words = "fixme|afaire"
    assertErrors(
      List(
        columnError(5, 2, List(words)),
        columnError(9, 4, List(words)),
        columnError(12, 2, List(words)),
        columnError(17, 3, List(words))
      ),
      source, params = Map("words" -> words))
  }
}
