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

  @Test def testOnelineFilter(): Unit = {
    val source = """
// scalastyle:ignore
 // scalastyle:ignore test
some code //   scalastyle:ignore     
"""
  val expected = List( CommentFilter( None         , Some(LineColumn(2,0)), Some(LineColumn(2,20)) ) 
                     , CommentFilter( Some("test") , Some(LineColumn(3,0)), Some(LineColumn(3,26)) )
                     , CommentFilter( None         , Some(LineColumn(4,0)), Some(LineColumn(4,37)) )
                     )
  assertCommentFilter(expected, source)
  }



  private[this] def assertCommentFilter(expected: List[CommentFilter], text: String) = {
    val hiddenTokenInfo = Checker.parseScalariform(text).get.comments
    val lines = Checker.parseLines(text)
    assertEquals(expected, CommentFilter.findCommentFilters(hiddenTokenInfo, lines))
  }
}
