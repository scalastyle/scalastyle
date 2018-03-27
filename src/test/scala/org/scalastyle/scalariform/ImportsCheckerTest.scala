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

import org.junit.Assert.assertTrue
import org.junit.Test
import org.scalastyle.file.CheckerTest
import org.scalatest.junit.AssertionsForJUnit

// scalastyle:off magic.number multiple.string.literals

class IllegalImportsCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "illegal.imports"
  val classUnderTest = classOf[IllegalImportsChecker]

  @Test def testNone(): Unit = {
    val source = """
package foobar

import java.util._

object Foobar {
  val foo = 1
}
"""

    assertErrors(List(), source)
  }

  @Test def testDefault(): Unit = {
    val source = """package foobar

import java.util._
import sun.com.foobar
import sun._

object Foobar {
}
""".stripMargin

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
""".stripMargin

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
""".stripMargin

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
                 """.stripMargin

    assertErrors(List(columnError(5, 0), columnError(6, 0), columnError(7, 0), columnError(8, 0)), source,
      Map("illegalImports" -> "java.util._", "exemptImports" -> "java.util.List"))
  }
}


class UnderscoreImportCheckerTest extends AssertionsForJUnit with CheckerTest {
  val key = "underscore.import"
  val classUnderTest = classOf[UnderscoreImportChecker]

  @Test def testWithoutExceptions(): Unit = {
    val source = """
package foobar

import java.util.List
import java.util._
import java.util.{_}
import java.util.{Foo => Bar, _}

object Foobar {
  import scala._
}
"""

    assertErrors(List(columnError(5, 0), columnError(6, 0), columnError(7, 0), columnError(10, 2)), source)
  }

  @Test def testWithExceptions(): Unit = {
    val source = """
package foobar

import java.util.List
import java.lang._
import collection.JavaConverters._
import scala.concurrent.duration.{DAYS, _}

object Foobar {
  import collection.JavaConverters._
}
"""
    val params = Map("ignoreRegex" -> "collection\\.JavaConverters\\._|scala\\.concurrent\\.duration\\._")
    val expected = List(columnError(5, 0))

    assertErrors(expected, source, params = params)
  }
}

class ImportGroupingCheckerTest extends AssertionsForJUnit with CheckerTest {
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
"""

    assertErrors(List(columnError(9, 2), columnError(12, 0), columnError(15, 2)), source)
  }


  @Test def testNone(): Unit = {
    val source = """
package foobar

object Foobar {
}

object Barbar {
}
"""

    assertErrors(List(), source)
  }
}

class ImportOrderCheckerTest extends AssertionsForJUnit with CheckerTest {
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
  val paramsLexicographic =
    params ++ Seq(
      "lexicographic" -> "true"
      )
  val paramsIntellij =
    params ++ Seq(
      "intellij" -> "true"
      )

  @Test def testNameComparison(): Unit = {
    val checker = new ImportOrderChecker()

    // Rules for import statements.
    assertTrue("import: wildcard before class", checker.compareNames("_", "Class", true) < 0)
    assertTrue("import: wildcard before package", checker.compareNames("_", "package", true) < 0)
    assertTrue("import: package after wildcard", checker.compareNames("package", "_", true) > 0)
    assertTrue("import: package after class", checker.compareNames("package", "Class", true) > 0)
    assertTrue("import: class before package", checker.compareNames("Class", "package", true) < 0)
    assertTrue("import: classes in alphabetical order",
      checker.compareNames("Class1", "Class2", true) < 0)
    assertTrue("import: classes in case-insensitive alphabetical order",
      checker.compareNames("Classone", "ClassTwo", true) < 0)
    assertTrue("import: packages in alphabetical order",
      checker.compareNames("package2", "package1", true) > 0)
    assertTrue("import: packages in case-insensitive alphabetical order",
      checker.compareNames("packagelower2", "packageUpper1", true) < 0)

    // Rules for names inside selectors.
    assertTrue("selector: wildcard after class", checker.compareNames("_", "Class", false) > 0)
    assertTrue("selector: package before class",
      checker.compareNames("package", "Class", false) < 0)
    assertTrue("selector: class after package", checker.compareNames("Class", "package", false) > 0)
    assertTrue("selector: classes in alphabetical order",
      checker.compareNames("Class1", "Class2", false) < 0)
    assertTrue("selector: packages in alphabetical order",
      checker.compareNames("package2", "package1", false) > 0)
  }

  @Test def testLexicographicNameComparison(): Unit = {
    val checker = new ImportOrderChecker()
    checker.setParameters(paramsLexicographic)

    // Rules for import statements.
    assertTrue("import: wildcard after class", checker.compareNames("_", "Class", true) > 0)
    assertTrue("import: wildcard before package", checker.compareNames("_", "package", true) < 0)
    assertTrue("import: package after wildcard", checker.compareNames("package", "_", true) > 0)
    assertTrue("import: package after class", checker.compareNames("package", "Class", true) > 0)
    assertTrue("import: class before package", checker.compareNames("Class", "package", true) < 0)
    assertTrue("import: classes in alphabetical order",
      checker.compareNames("Class1", "Class2", true) < 0)
    assertTrue("import: classes in case-sensitive alphabetical order",
      checker.compareNames("Classone", "ClassTwo", true) > 0)
    assertTrue("import: packages in alphabetical order",
      checker.compareNames("package2", "package1", true) > 0)
    assertTrue("import: packages in case-sensitive alphabetical order",
      checker.compareNames("packagelower2", "packageUpper1", true) > 0)

    // Rules for names inside selectors.
    assertTrue("selector: wildcard after class", checker.compareNames("_", "Class", false) > 0)
    assertTrue("selector: package before class",
      checker.compareNames("package", "Class", false) < 0)
    assertTrue("selector: class after package", checker.compareNames("Class", "package", false) > 0)
    assertTrue("selector: classes in alphabetical order",
      checker.compareNames("Class1", "Class2", false) < 0)
    assertTrue("selector: packages in alphabetical order",
      checker.compareNames("package2", "package1", false) > 0)
  }

  @Test def testImportComparison(): Unit = {
    val checker = new ImportOrderChecker()

    assertTrue("package after class", checker.compareImports("foo.package", "foo.Class") > 0)
    assertTrue("class before package", checker.compareImports("foo.Class", "foo.package") < 0)
    assertTrue("selector before subpackage", checker.compareImports("foo.", "foo.package") < 0)
    assertTrue("wildcard before selector", checker.compareImports("foo._", "foo.") < 0)
    assertTrue("wildcard before subpackage", checker.compareImports("foo._", "foo.package") < 0)
  }

  @Test def testLexicographicImportComparison(): Unit = {
    val checker = new ImportOrderChecker()
    checker.setParameters(paramsLexicographic)

    assertTrue("package after class", checker.compareImports("foo.package", "foo.Class") > 0)
    assertTrue("class before package", checker.compareImports("foo.Class", "foo.package") < 0)
    assertTrue("selector before subpackage", checker.compareImports("foo.", "foo.package") < 0)
    assertTrue("wildcard before selector", checker.compareImports("foo._", "foo.") < 0)
    assertTrue("wildcard before subpackage", checker.compareImports("foo._", "foo.package") < 0)
  }

  @Test def testSubPackage(): Unit = {
    val source = """
      |import foobar.subpackage.Foo
      |import foobar.Bar
      """.stripMargin

    val expected = List(
      columnError(3, 0, errorKey = errorKey("wrongOrderInGroup"),
        args = List("foobar.Bar", "foobar.subpackage.Foo")))

    assertErrors(expected, source, params = params)
  }

  @Test def testInGroupOrdering(): Unit = {
    val source = """
      |import java.nio.ByteBuffer
      |import java.nio.file.{Paths, Files}
      """.stripMargin

    val expected = List(
      columnError(3, 21, errorKey = errorKey("wrongOrderInSelector"),
        args = List("Files", "Paths")))

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
      """.stripMargin

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

  @Test def testLexicographicImportGrouping(): Unit = {
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
      |import my.org.project3.Someclass
      |import my.org.project3.SomeClass
      |import second.org.{MyClass}
      |import second.org.subpackage.MyClass
      |
      |
      |import my.org.project2.OtherClass
      |import javax.swing.JTree
      |
      |object Foobar {
      |}
      """.stripMargin

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
      columnError(16, 0, errorKey = errorKey("wrongOrderInGroup"),
        args = List("my.org.project3.SomeClass", "my.org.project3.Someclass")),
      columnError(22, 0, errorKey = errorKey("wrongGroup"),
        args = List("javax.swing.JTree", "java", "project2"))
    )

    assertErrors(expected, source, params = paramsLexicographic)
  }

  @Test def testIntellijImportGrouping(): Unit = {
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
      |import my.org.project3.Someclass
      |import my.org.project3.SomeClass
      |import second.org.{MyClass}
      |import second.org.subpackage.MyClass
      |
      |
      |import my.org.project2.OtherClass
      |import javax.swing.JTree
      |
      |object Foobar {
      |}
      """.stripMargin

    val expected = List(
      columnError(5, 20, errorKey = errorKey("wrongOrderInSelector"), args = List("Cipher", "Mac")),
      columnError(6, 0, errorKey = errorKey("wrongOrderInGroup"),
        args = List("java.lang.{Long, Boolean}", "javax.crypto.{Mac, Cipher}")),
      columnError(6, 17, errorKey = errorKey("wrongOrderInSelector"),
        args = List("Boolean", "Long")),
      columnError(7, 0, errorKey = errorKey("wrongOrderInGroup"),
        args = List("java.lang._", "java.lang.{Long, Boolean}")),
      columnError(9, 0, errorKey = errorKey("noEmptyLine")),
      columnError(10, 0, errorKey = errorKey("missingEmptyLine"), args = List("java", "scala")),
      columnError(14, 0, errorKey = errorKey("wrongOrderInGroup"), args = List("my.org.project1.MyClass", "org.apache.Foo")),
      columnError(18, 0, errorKey = errorKey("wrongOrderInGroup"), args = List("second.org.subpackage.MyClass", "second.org.{MyClass}")),
      columnError(22, 0, errorKey = errorKey("wrongGroup"), args = List("javax.swing.JTree", "java", "project2"))
    )

    assertErrors(expected, source, params = paramsIntellij)
  }

  private def errorKey(subkey: String): Option[String] = Some(key + "." + subkey)

}
