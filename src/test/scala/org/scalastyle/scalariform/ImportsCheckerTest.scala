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


class UnderscoreImportCheckerTest extends AssertionsForJUnit with CheckerTest {
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
