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

import org.scalastyle.file.CheckerTestHelper
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.scalastyle.Checker
import org.scalastyle.StyleError
import java.util.Set
import org.junit.Before
import org.junit.Test

// scalastyle:off magic.number multiple.string.literals

class IllegalImportsCheckerTest extends AssertionsForJUnit with CheckerTestHelper {
  val key = "illegal.imports"
  val classUnderTest = classOf[IllegalImportsChecker]

  @Test def testNone(): Unit = {
    val source = """
package foobar

import java.util._

object Foobar {
  val foo = 1
}
""";

    assertErrors(List(), source)
  }

  @Test def testDefault(): Unit = {
    val source = """package foobar

import java.util._
import sun.com.foobar;
import sun._

object Foobar {
}
""".stripMargin;

    assertErrors(List(columnError(4, 0), columnError(5, 0)), source)
  }

  @Test def testRenamingWildcard(): Unit = {
    val source = """package foobar

import java.util.{List => JList}
import java.lang.{Object => JObject}
import java.util.{List,Map}
import java.util.{_}
import java.util._

object Foobar {
}
""".stripMargin;

    assertErrors(List(columnError(3, 0), columnError(5, 0), columnError(6, 0), columnError(7, 0)), source, Map("illegalImports" -> "java.util._"))
  }

  @Test def testRenamingSpecific(): Unit = {
    val source = """package foobar

import java.util.{List => JList}
import java.lang.{Object => JObject}
import java.util.{Iterator => JIterator, List => JList, Collection => JCollection}
import java.util.{List, Map}
import java.util.{_}
import java.util._

object Foobar {
}
""".stripMargin;

    assertErrors(List(columnError(3, 0), columnError(5, 0), columnError(6, 0)), source,
        Map("illegalImports" -> "java.util.List, java.util.Map"))
  }

  @Test def testWithExemptImports(): Unit = {
    val source = """package foobar

import java.util.{List => JList}
import java.lang.{Object => JObject}
import java.util.{Iterator => JIterator, List => JList, Collection => JCollection}
import java.util.{List, Map}
import java.util.{_}
import java.util._

object Foobar {
}
                 """.stripMargin;

    assertErrors(List(columnError(5, 0), columnError(6, 0), columnError(7, 0), columnError(8, 0)), source,
      Map("illegalImports" -> "java.util._", "exemptImports" -> "java.util.List"))
  }
}


class UnderscoreImportCheckerTest extends AssertionsForJUnit with CheckerTestHelper {
  val key = "underscore.import"
  val classUnderTest = classOf[UnderscoreImportChecker]

  @Test def testNone(): Unit = {
    val source = """
package foobar

import java.util.List
import java.util._
import java.util.{_}
import java.util.{Foo => Bar, _}

object Foobar {
  import scala._
}
""";

    assertErrors(List(columnError(5, 0), columnError(6, 0), columnError(7, 0), columnError(10, 2)), source)
  }
}

class ImportGroupingCheckerTest extends AssertionsForJUnit with CheckerTestHelper {
  val key = "import.grouping"
  val classUnderTest = classOf[ImportGroupingChecker]

  @Test def testKO(): Unit = {
    val source = """
package foobar

import java.util.List;
import java.util._ // here is a comment
import java.util._

object Foobar {
  import java.util.Map
}

import java.util.Collection

object Barbar {
  import java.util.HashMap
}
""";

    assertErrors(List(columnError(9, 2), columnError(12, 0), columnError(15, 2)), source)
  }


  @Test def testNone(): Unit = {
    val source = """
package foobar

object Foobar {
}

object Barbar {
}
""";

    assertErrors(List(), source)
  }
}

class ImportOrderCheckerTest extends AssertionsForJUnit with CheckerTestHelper {
  val key = "import.ordering"
  val classUnderTest = classOf[ImportOrderChecker]

  val params = Map(
    "groups" -> "java,scala,others,project2",
    "group.java" -> "javax?\\..+",
    "group.scala" -> "scala\\..+",
    "group.others" -> "(?!my\\.org\\.project2\\.).*",
    "group.project2" -> "my\\.org\\.project2\\..*",
    "maxBlankLines" -> "2"
    )

  @Test def testSubPackage(): Unit = {
    val source = """
      |package foobar
      |
      |import foobar.subpackage.Foo
      |import foobar.Bar
      |
      |object Foobar {
      |}
      """.stripMargin;

    val expected = List(
      columnError(5, 0, errorKey = errorKey("wrongOrderInGroup"),
        args = List("foobar.Bar", "foobar.subpackage.Foo")))

    assertErrors(expected, source, params = params)
  }

  @Test def testImportGrouping(): Unit = {
    val source = """
      |package foobar
      |
      |import java.util.Map
      |import javax.crypto.{Mac, Cipher}
      |import java.lang.{Long => JLong, Boolean => JBoolean}
      |import java.lang._
      |
      |import java.security.Permission
      |import scala.io.Source
      |
      |
      |import org.apache.Foo
      |import my.org.project1.MyClass
      |
      |
      |
      |import my.org.project2.OtherClass
      |import javax.swing.JTree
      |
      |object Foobar {
      |}
      """.stripMargin;

    val expected = List(
      columnError(5, 20, errorKey = errorKey("wrongOrderInSelector"), args = List("Cipher", "Mac")),
      columnError(6, 0, errorKey = errorKey("wrongOrderInGroup"),
        args = List("java.lang.", "javax.crypto.")),
      columnError(6, 17, errorKey = errorKey("wrongOrderInSelector"),
        args = List("Boolean", "Long")),
      columnError(7, 0, errorKey = errorKey("wrongOrderInGroup"),
        args = List("java.lang._", "java.lang.")),
      columnError(9, 0, errorKey = errorKey("noEmptyLine")),
      columnError(10, 0, errorKey = errorKey("missingEmptyLine"), args = List("java", "scala")),
      columnError(14, 0, errorKey = errorKey("wrongOrderInGroup"),
        args = List("my.org.project1.MyClass", "org.apache.Foo")),
      columnError(18, 0, errorKey = errorKey("tooManyEmptyLines"),
        args = List("2", "others", "project2")),
      columnError(19, 0, errorKey = errorKey("wrongGroup"),
        args = List("javax.swing.JTree", "java", "project2"))
      )

    assertErrors(expected, source, params = params)
  }

  private def errorKey(subkey: String): Option[String] = Some(key + "." + subkey)

}
