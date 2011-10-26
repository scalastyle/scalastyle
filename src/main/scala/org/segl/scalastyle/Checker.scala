package org.segl.scalastyle

object ScalastyleChecker {
  val checkers: List[Class[_ <: Checker]] = List(classOf[FileTabChecker],
		  classOf[FileLineLengthChecker],
		  classOf[FileLengthChecker],
		  classOf[SpacesAfterPlusChecker])
}

class ScalastyleChecker {
  def checkFiles(files: List[String]): List[Message] = {
    StartWork() :: files.flatMap(file => Checker.verifyFile(ScalastyleChecker.checkers, file)).toList ::: List(EndWork()) 
  }
}