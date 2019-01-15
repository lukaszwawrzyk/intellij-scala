package org.jetbrains.plugins.scala.compiler

import com.intellij.openapi.compiler.{CompileContext, CompileTask, CompilerManager, CompilerMessageCategory}
import com.intellij.openapi.components._
import com.intellij.openapi.module.ModuleManager
import com.intellij.openapi.project.Project
import com.intellij.util.xmlb.XmlSerializerUtil
import org.jetbrains.plugins.scala.project.IncrementalityType
import org.jetbrains.plugins.scala.project.settings.ScalaCompilerConfiguration

import scala.beans.BeanProperty

@State(
  name = "ScalaCompilerConfiguration",
  storages = Array(new Storage("scala_incrementality.xml"))
)
class IncrementalityState extends PersistentStateComponent[IncrementalityState] {
  override def loadState(state: IncrementalityState): Unit = XmlSerializerUtil.copyBean(state, this)

  override def getState: IncrementalityState = this

  @BeanProperty var lastIncrementalityType: String = _
}

class IncrementalityChecker(project: Project) extends AbstractProjectComponent(project) {
  val errorMsg = "scala: type of incremental compiler has been changed, full rebuild is required"

  private object Checker extends CompileTask {
    override def execute(compileContext: CompileContext): Boolean = {
      val currentType = ScalaCompilerConfiguration.instanceIn(project).incrementalityType
      val state = ServiceManager.getService(project, classOf[IncrementalityState])
      val previousType = Option(state.lastIncrementalityType).map(IncrementalityType.valueOf)

      def isRebuild = !compileContext.isMake &&
        compileContext.getCompileScope.getAffectedModules.toSet == ModuleManager.getInstance(project).getModules.toSet

      if (previousType.exists(_ != currentType) && !isRebuild) {
        compileContext.addMessage(CompilerMessageCategory.ERROR, errorMsg, null, -1, -1)
        false
      } else {
        state.lastIncrementalityType = currentType.getName
        true
      }
    }
  }

  override def projectOpened(): Unit =
    CompilerManager.getInstance(project).addBeforeTask(Checker)
}
