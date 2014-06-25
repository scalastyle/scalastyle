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

package org.scalastyle

// scalastyle:off magic.number multiple.string.literals

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class CommentFilterTest extends AssertionsForJUnit {
  @Test def testTokens(): Unit = {
    val text = """
// scalastyle:off
      // another comment
// scalastyle:on"""
    val comments = Checker.parseScalariform(text).get.comments

    val tokens = CommentFilter.findScalastyleComments(comments)

    assertEquals(2, tokens.size)
  }

  @Test def testOffOn(): Unit = {
    assertCommentFilter(List(CommentFilter(None, Some(LineColumn(2, 0)),Some(LineColumn(3, 0)))), """
// scalastyle:off
// scalastyle:on""")
  }

  @Test def testOffOnVariousWhitespace(): Unit = {
    assertCommentFilter(List(CommentFilter(None, Some(LineColumn(2, 0)),Some(LineColumn(3, 1)))), """
//  scalastyle:off
 //  scalastyle:on """)
  }

  @Test def testOffOnIds(): Unit = {
    assertCommentFilter(List(CommentFilter(Some("magic.number"), Some(LineColumn(2, 0)),Some(LineColumn(4, 0))),
        CommentFilter(Some("class.name"), Some(LineColumn(3, 0)),Some(LineColumn(5, 1)))), """
//  scalastyle:off magic.number
//  scalastyle:off class.name
//  scalastyle:on magic.number
 //  scalastyle:on class.name""")
  }

  @Test def testOffOnMultipleIds(): Unit = {
    assertCommentFilter(List(CommentFilter(Some("magic.number"), Some(LineColumn(2, 0)),Some(LineColumn(4, 0))),
        CommentFilter(Some("class.name"), Some(LineColumn(3, 0)),Some(LineColumn(5, 1))),
        CommentFilter(Some("object.name"), Some(LineColumn(2, 0)), None)), """
//  scalastyle:off magic.number object.name
//  scalastyle:off class.name
//  scalastyle:on magic.number
 //  scalastyle:on class.name""")
  }

  @Test def testOffOnOpenEnds(): Unit = {
    assertCommentFilter(List(CommentFilter(Some("magic.number"), Some(LineColumn(2, 0)),Some(LineColumn(3, 0))),
        CommentFilter(Some("object.name"), Some(LineColumn(5, 1)), None),
        CommentFilter(Some("class.name"), Some(LineColumn(2, 0)), None)), """
//  scalastyle:off magic.number class.name
//  scalastyle:on magic.number
//  scalastyle:on magic.number
 //  scalastyle:off object.name
""")
  }

  @Test def testOneLine(): Unit = {
    val source = """
// scalastyle:ignore
 // scalastyle:ignore test
some code //   scalastyle:ignore
"""
  val expected = List( CommentFilter(None         ,Some(LineColumn(2,0)), Some(LineColumn(3,0)) )
                     , CommentFilter(Some("test") ,Some(LineColumn(3,0)), Some(LineColumn(4,0)) )
                     , CommentFilter(None         ,Some(LineColumn(4,0)), Some(LineColumn(5,0)) )
                     )
  assertCommentFilter(expected, source)
  }


  @Test def testCombination(): Unit = {
    val source = """
// scalastyle:off magic.number
// scalastyle:ignore magic.number
// scalastyle:on magic.number
// scalastyle:ignore magic.number
// scalastyle:off magic.number
"""
  val expected = List(CommentFilter(Some("magic.number"), Some(LineColumn(3,0)), Some(LineColumn(4,0)))
                     , CommentFilter(Some("magic.number"), Some(LineColumn(5,0)), Some(LineColumn(6,0)))
                     , CommentFilter(Some("magic.number"), Some(LineColumn(2,0)), Some(LineColumn(4,0)))
                     , CommentFilter(Some("magic.number"), Some(LineColumn(6,0)), None)
                     )
  assertCommentFilter(expected, source)
  }


  @Test def testCombination2(): Unit = {
    val source = """
package foobar

class foobar {
  // scalastyle:on class.name
  class barbar1 { } // scalastyle:ignore class.name
  //

  // scalastyle:on
  class barbar2 { } // scalastyle:ignore
  // scalastyle:off

  // scalastyle:on
  class barbar3 { } // scalastyle:ignore class.name
  // scalastyle:off

  // scalastyle:on
  class barbar4 { } // scalastyle:ignore magic.number
  // scalastyle:off
}
""";
  val expected = List( CommentFilter(Some("class.name"),   Some(LineColumn(6,0)), Some(LineColumn(7,0)))
                     , CommentFilter(None              ,   Some(LineColumn(10,0)), Some(LineColumn(11,0)))
                     , CommentFilter(Some("class.name"),   Some(LineColumn(14,0)), Some(LineColumn(15,0)))
                     , CommentFilter(Some("magic.number"), Some(LineColumn(18,0)), Some(LineColumn(19,0)))
                     , CommentFilter(None,                 Some(LineColumn(11,2)), Some(LineColumn(13,2)))
                     , CommentFilter(None,                 Some(LineColumn(15,2)), Some(LineColumn(17,2)))
                     , CommentFilter(None,                 Some(LineColumn(19,2)), None)
                     )
  assertCommentFilter(expected, source)
  }

  private[this] def assertCommentFilter(expected: List[CommentFilter], text: String) = {
    val hiddenTokenInfo = Checker.parseScalariform(text).get.comments
    val lines = Checker.parseLines(text)
    assertEquals(expected.mkString("\n"), CommentFilter.findCommentFilters(hiddenTokenInfo, lines).mkString("\n"))
  }
}
