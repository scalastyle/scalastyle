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

import java.io.File
import java.io.FileFilter
import scala.collection.JavaConversions.seqAsJavaList
import scala.collection.JavaConversions.collectionAsScalaIterable

class Directory

class DirectoryFileSpec(name: String, encoding: Option[String], val file: java.io.File) extends RealFileSpec(name, encoding) {
  override def toString: String = file.getAbsolutePath()
}

object Directory {
  val scalaFileFilter = new FileFilter() {
    def accept(file: File): Boolean = file.getAbsolutePath().endsWith(".scala")
  }

  def getFilesAsJava(encoding: Option[String], files: java.util.List[File], excludedFiles: Option[String] = None): java.util.List[FileSpec] = {
    val exclusionPatterns = excludedFiles.map(createFileExclusionFilter(_))
    seqAsJavaList(privateGetFiles(encoding, collectionAsScalaIterable(files)))
  }

  def getFiles(encoding: Option[String], files: Iterable[File], excludedFiles: Option[String] = None): List[FileSpec] = {
    val excludeFilter = excludedFiles.map(createFileExclusionFilter(_))
    privateGetFiles(encoding, files, excludeFilter)
  }

  private[this] def createFileExclusionFilter(excludedFiles: String): FileFilter = {
    val exclusionPatterns = excludedFiles.split(";").map(_.r)
    new FileFilter {
      def accept(file: File): Boolean = {
        val path = file.getAbsolutePath
        exclusionPatterns.exists(_.findFirstMatchIn(path).isDefined)
      }
    }
  }

  private[this] def privateGetFiles(encoding: Option[String], files: Iterable[File], excludeFilter: Option[FileFilter] = None): List[FileSpec] = {
    files.flatMap(f => {
      if (excludeFilter.exists(_.accept(f))) {
        Nil
      } else if (f.isDirectory) {
        privateGetFiles(encoding, f.listFiles, excludeFilter)
      } else if (scalaFileFilter.accept(f)) {
        List(new DirectoryFileSpec(f.getAbsolutePath, encoding, f.getAbsoluteFile))
      } else {
        Nil
      }
    }).toList
  }
}




