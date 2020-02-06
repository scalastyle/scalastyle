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

import scala.jdk.CollectionConverters._

class Directory

class DirectoryFileSpec(name: String, encoding: Option[String], val file: java.io.File)
    extends RealFileSpec(name, encoding) {
  override def toString: String = file.getAbsolutePath
}

object Directory {
  val scalaFileFilter = new FileFilter() {
    def accept(file: File): Boolean = file.getAbsolutePath.endsWith(".scala")
  }

  def getFilesAsJava(encoding: Option[String], files: java.util.List[File]): java.util.List[FileSpec] =
    privateGetFiles(encoding, files.asScala).asJava

  def getFiles(
    encoding: Option[String],
    files: Iterable[File],
    excludedFiles: Seq[String] = Nil
  ): List[FileSpec] = {
    val excludeFilter = createFileExclusionFilter(excludedFiles)
    privateGetFiles(encoding, files, excludeFilter).toList
  }

  private[this] def createFileExclusionFilter(excludedFiles: Seq[String]): Option[FileFilter] = {
    if (excludedFiles.isEmpty) {
      None
    } else {
      val exclusionPatterns = excludedFiles.map(_.r)
      Some(new FileFilter {
        def accept(file: File): Boolean = {
          val path = file.getAbsolutePath
          exclusionPatterns.exists(_.findFirstMatchIn(path).isDefined)
        }
      })
    }
  }

  private[this] def privateGetFiles(
    encoding: Option[String],
    files: Iterable[File],
    excludeFilter: Option[FileFilter] = None
  ): Seq[FileSpec] = {

    def getFilesHelper(currentFiles: Iterable[File], acc: Set[File]): Set[File] = {
      currentFiles.headOption match {
        case Some(f) =>
          if (excludeFilter.exists(_.accept(f))) {
            getFilesHelper(currentFiles.tail, acc)
          } else if (f.isDirectory) {
            val newCurrentFiles = currentFiles.tail ++ f.listFiles
            getFilesHelper(newCurrentFiles, acc)
          } else if (scalaFileFilter.accept(f) && !acc(f)) {
            val newAcc = acc + f
            getFilesHelper(currentFiles.tail, newAcc)
          } else {
            getFilesHelper(currentFiles.tail, acc)
          }
        case None => acc
      }
    }

    val uniqueFiles = getFilesHelper(files, Set.empty)
    uniqueFiles.toSeq.map(f => new DirectoryFileSpec(f.getAbsolutePath, encoding, f.getAbsoluteFile))
  }

}
