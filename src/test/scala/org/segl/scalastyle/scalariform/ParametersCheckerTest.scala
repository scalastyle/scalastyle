package org.segl.scalastyle.scalariform

import org.scalatest.junit.AssertionsForJUnit
import org.segl.scalastyle.file.CheckerTest
import org.junit.Test
import scalariform.lexer.Token


class ParametersCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "nb.of.parameter"
  val classUnderTest = classOf[ParametersChecker]


  @Test def testGoodParameters() = {
    val source = """
    package foobar

    object Foobar {
      def doo(list: List[Message], source: String)
    }
    """
    assertErrors(List(),source, Map("maxParamNumber" -> "2"))

  }
  @Test def testGoodParamatrizedParameters() = {
    val source = """
    package foobar

    object Foobar {
      def doo[Ti,To](list: Int, source: Ti) {}
    }
    """
    assertErrors(List(),source, Map("maxParamNumber" -> "2"))

  }
  @Test def testZeroParam() = {
    val source = """
    package foobar

    object Foobar {
      def doo = "toto"
      def doo() = 'symb
    }
    """
    assertErrors(List(),source, Map("maxParamNumber" -> "0"))

  }
@Test def testZeroParametrisedParam() = {
    val source = """
    package foobar

    object Foobar {
      def doo[T] = "toto".asInstanceOf[T]
      def doo[T]() = "doo".asInstanceOf[T]
    }
    """
    assertErrors(List(),source, Map("maxParamNumber" -> "0"))

  }

  @Test def testTooManyParameters() = {
    val source = """
    package foobar

   object Foobar {
       def doo(list: List[Message], source1: String, source2: String, source3: String, source4: String, source5: String, source6: String)
    }
    """
      assertErrors(List(positionError(54)), source, Map("maxParamNumber" -> "2"))
	}

  @Test def testTooManyParamatrizedParameters() = {
    val source = """
    package foobar

    object Foobar {
      def doo[Ti,To](list: Int, source: Ti) {}
    }
    """
    assertErrors(List(positionError(54)),source, Map("maxParamNumber" -> "1"))

  }

  @Test def testTooManyParametersMultiple() = {
    val source = """
    package foobar

   object Foobar {
       def doo(list: List[Message], source1: String, source2: String, source3: String, source4: String, source5: String, source6: String)
       def foo()
       def poo = {}
       def dude(list: List[Message], source1: String, source2: String, source3: String, source4: String, source5: String, source6: String)
       def last = doo
    }
    """
      assertErrors(List(positionError(54),positionError(230)), source, Map("maxParamNumber" -> "6"))
	}
}