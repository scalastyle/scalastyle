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

import org.scalastyle.CombinedMeta
import org.scalastyle.CombinedMetaChecker
import org.scalastyle.ScalastyleError

import scala.meta.Import
import scala.meta.Importee

class BlockImportChecker extends CombinedMetaChecker {
  val errorKey = "block.import"

  def verify(ast: CombinedMeta): List[ScalastyleError] = {
    SmVisitor.getAll[Import](ast.tree).filter(isBlockImport).map(_.importers.head).map(toError)
  }

  private def isBlockImport(i: Import): Boolean = {
    val count = countTypes(i)

    if (count.wildcard > 0) {
      // only wildcard is not a block import
      if (count.rename + count.name + count.unimport == 0) {
        false
      } else if (count.rename > 0) {
        // wildcard with rename is block import if there is a name as well
        count.name > 0
      } else {
        // otherwise block import
        true
      }
    } else {
      // if there are multiple imports, we have a block import
      count.name > 1
    }
  }

  case class Counts(name: Int, rename: Int, unimport: Int, wildcard: Int)

  private def countTypes(i: Import): Counts = {
    var name = 0
    var rename = 0
    var unimport = 0
    var wildcard = 0

    i.importers.foreach { importer =>
      importer.importees.foreach {
        case i: Importee.Name     => name = name + 1
        case i: Importee.Rename   => rename = rename + 1
        case i: Importee.Unimport => unimport = unimport + 1
        case i: Importee.Wildcard => wildcard = wildcard + 1
        case _                    => // nothing
      }
    }

    Counts(name, rename, unimport, wildcard)
  }
}
