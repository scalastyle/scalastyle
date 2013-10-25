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

import org.scalastyle.file.CheckerTest
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.scalastyle.Checker
import org.scalastyle.StyleError
import java.util.Set
import org.junit.Before
import org.junit.Test

// scalastyle:off magic.number

class EqualsHashCodeCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "equals.hash.code"
  val classUnderTest = classOf[EqualsHashCodeChecker]

  @Test def testOK(): Unit = {
    val source = """
package foobar

class OK {
  def hashCode(): Int = 45
  def equals(o: java.lang.Object): Boolean = false
}
""";

    assertErrors(List(), source)
  }

  @Test def testHashCodeOnlyKO(): Unit = {
    val source = """
package foobar

class HashCodeOnlyKO {
  def hashCode(): Int = 45
}
""";

    assertErrors(List(columnError(4, 6)), source)
  }

  @Test def testEqualsOnlyKO(): Unit = {
    val source = """
package foobar

class EqualsOnlyKO {
  def equals(o: java.lang.Object): Boolean = false
}
""";

    assertErrors(List(columnError(4, 6)), source)
  }

  @Test def testEqualsOnlyAnyKO(): Unit = {
    val source = """
package foobar

class EqualsOnlyKO {
  def equals(o: Any): Boolean = false
}
""";

    assertErrors(List(columnError(4, 6)), source)
  }

  @Test def testEqualsWrongSignatureOK(): Unit = {
    val source = """
package foobar

class EqualsWrongSignatureOK {
  def equals(o: scala.Object): Boolean = false
  def equals(o: java.lang.Object)(o2: java.lang.Object): Boolean = false
}
""";

    assertErrors(List(), source)
  }

  @Test def testHashCodeWrongSignatureOK(): Unit = {
    val source = """
package foobar

class HashCodeWrongSignatureOK {
  def hashCode(o: scala.Object): Int = 45
}
""";

    assertErrors(List(), source)
  }

  @Test def testOuterKOInnerKO(): Unit = {
    val source = """
package foobar

class OuterKO {
  def hashCode(): Int = 45
  class InnerKO {
    def equals(o: java.lang.Object): Boolean = false
  }
}
""";

    assertErrors(List(columnError(4, 6), columnError(6, 8)), source)
  }

  @Test def testOuterOKInnerKO(): Unit = {
    val source = """
package foobar

class OuterOK {
  def hashCode(): Int = 45
  class InnerKO {
    def equals(o: java.lang.Object): Boolean = false
  }
  def equals(o: java.lang.Object): Boolean = false
}
""";

    assertErrors(List(columnError(6, 8)), source)
  }

  @Test def testObjectInnerKO(): Unit = {
    val source = """
package foobar

object Object {
  class ObjectInnerKO {
    def equals(o: java.lang.Object): Boolean = false
  }
}
""";

    assertErrors(List(columnError(5, 8)), source)
  }

  @Test def testMultipleClasses(): Unit = {
    val source = """
package foobar

class Class1 {
  def hashCode(): Int = 45
}

class Class2 {
  def hashCode(): Int = 45
}
""";

    assertErrors(List(columnError(4, 6), columnError(8, 6)), source)
  }
}
