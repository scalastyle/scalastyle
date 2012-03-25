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

import java.io._
import scala.xml._

class Directory

class DirectoryFileSpec(val name: String, val file: java.io.File) extends FileSpec {
  override def toString = file.getAbsolutePath()
}

object Directory {
  val scalaFileFilter = new FileFilter() {
    def accept(file: File): Boolean = file.getAbsolutePath().endsWith(".scala")
  }

  def getFiles(dirs: File*): List[FileSpec] = {
    dirs.map(dir => {
        dir.listFiles(scalaFileFilter).map(f => new DirectoryFileSpec(f.getAbsolutePath(), f.getAbsoluteFile())).toList :::
                                              dir.listFiles().filter(_.isDirectory).flatMap(getFiles(_)).toList
    }).flatten.toList
  }

  def main(args: Array[String]): Unit = {
    class Foo(bar: String, bar2: Object) {
    }

    toXml(new Foo("string", "string").getClass)
  }

  def toXml(c: Class[_]) = {
    for (field <- this.getClass.getDeclaredFields)
      "field name=" + field.getName + " tpe=" + field.getType.toString() + this.getClass.getMethods.find(_.getName() == field.getName).get.invoke(this)

  }
}

