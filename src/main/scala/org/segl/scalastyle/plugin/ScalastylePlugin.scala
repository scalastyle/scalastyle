package org.segl.scalastyle.plugin

import scala.tools.nsc
import nsc.Global
import nsc.plugins.Plugin
import nsc.Phase
import nsc.plugins.PluginComponent
import org.segl.scalastyle._;
import java.lang.reflect.Constructor
import scala.collection.mutable.ListBuffer

class ScalastylePlugin(val global: Global) extends Plugin {
  import global._

  override val name = "scalastyle"
  override val description = "checkstyle for Scala"
  override val components = List(new ScalastyleComponent(global, ScalastyleChecker.checkers))
}

class ScalastyleComponent(val global: Global, checkers: List[Class[_ <: Checker]]) extends PluginComponent {
  import global._
  
  override val runsAfter = List[String]("parser");
  override val runsRightAfter = Some("parser");
  override val phaseName = "scalastyle parser phase"
  override def newPhase(_prev: Phase) = new ParserPhase(_prev)

  class ParserPhase(prev: Phase) extends StdPhase(prev) {
    import java.lang.reflect.Constructor

    override def name = "scalastyle parser phase"
    override def apply(unit: CompilationUnit) {
      val checkerInstances = checkers.map(constructor(_).newInstance(global))
      val messages = ListBuffer[Message]()
      
      for (tree <- unit.body) {
        try {
          messages ++= checkerInstances.flatMap { _.verify(unit.source.file.file.getAbsolutePath(), tree) }
        } catch {
          case e => e.printStackTrace
        }
      }
      
      println("messages=" + messages.mkString("\n"))
      
    }
    
    def constructor(clazz: Class[_ <: Checker]) = clazz.getConstructor(classOf[Global]).asInstanceOf[Constructor[Checker]]
  }
}