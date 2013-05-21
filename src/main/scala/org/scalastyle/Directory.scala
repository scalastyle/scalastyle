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

  def getFilesAsJava(encoding: Option[String], files: java.util.List[File]): java.util.List[FileSpec] = {
    seqAsJavaList(privateGetFiles(encoding, collectionAsScalaIterable(files)))
  }

  def getFiles(encoding: Option[String], files: Iterable[File]): List[FileSpec] = {
    privateGetFiles(encoding, files);
  }

  private[this] def privateGetFiles(encoding: Option[String], files: Iterable[File]): List[FileSpec] = {
    files.map(f => {
      if (f.isDirectory) {
        getFiles(encoding, f.listFiles)
      } else if (scalaFileFilter.accept(f)) {
        List(new DirectoryFileSpec(f.getAbsolutePath(), encoding, f.getAbsoluteFile()))
      } else {
        List()
      }
    }).flatten.toList
  }

}

