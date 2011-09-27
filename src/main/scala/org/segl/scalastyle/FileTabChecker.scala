package org.segl.scalastyle

trait Checker {
  def verify(file: String, ast: AST): List[Message]
}

class FileTabChecker extends Checker {
  def verify(file: String, ast: AST): List[Message] = {
    for (line <- ast.lines.zipWithIndex;
    		if line._1.contains('\t')) yield {
      StyleError(file, "line.contains.tab", Some(line._2 + 1), Some(line._1.indexOf('\t')))
    }
  }
}

class FileLineLengthChecker extends Checker {
  def verify(file: String, ast: AST): List[Message] = {
    for (line <- ast.lines.zipWithIndex;
    		if line._1.length() > 80) yield {
      StyleError(file, "line.size.limit", Some(line._2 + 1))
    }
  }
}


class FileLengthChecker extends Checker {
  def verify(file: String, ast: AST): List[Message] = {
    if (ast.lines.size > 10) List(StyleError(file, "file.size.limit")) else List()
  }
}
