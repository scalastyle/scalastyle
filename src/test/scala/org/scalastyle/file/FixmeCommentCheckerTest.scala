package org.scalastyle.file

import org.junit.Test
import org.scalastyle.file.FixmeCommentChecker._
import org.scalatest.junit.AssertionsForJUnit

class FixmeCommentCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "fixme.comment"
  val classUnderTest = classOf[FixmeCommentChecker]

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
        columnError(5, 2, List(FixmeRegex)),
        columnError(9, 4, List(FixmeRegex)),
        columnError(12, 2, List(FixmeRegex)),
        columnError(15, 24, List(FixmeRegex)),
        columnError(17, 29, List(FixmeRegex)),
        columnError(19, 22, List(FixmeRegex)),
        columnError(22, 3, List(FixmeRegex)),
        columnError(23, 3, List(FixmeRegex)),
        columnError(26, 3, List(FixmeRegex)),
        columnError(27, 3, List(FixmeRegex))
      ),
      source)
  }
}
