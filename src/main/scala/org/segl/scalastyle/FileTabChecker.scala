package org.segl.scalastyle

trait Checker {
  def verify(file: String, ast: AST): List[Message]
}

class FileTabChecker extends Checker {
  def verify(file: String, ast: AST): List[Message] = {
    for (line <- ast.lines.zipWithIndex;
    		if line._1.contains("\t")) yield {
      new Message(file, "line.contains.tab", line._2 + 1, line._1.indexOf('\t'))
    }
  }
}

class FileLineLengthChecker extends Checker {
  def verify(file: String, ast: AST): List[Message] = {
    for (line <- ast.lines.zipWithIndex;
    		if line._1.length() > 80) yield {
      new Message(file, "line.size.limit", line._2 + 1, 0)
    }
  }
}
