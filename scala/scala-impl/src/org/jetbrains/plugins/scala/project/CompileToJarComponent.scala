package org.jetbrains.plugins.scala.project

import java.util.Collections

import com.intellij.ProjectTopics
import com.intellij.openapi.components.ProjectComponent
import com.intellij.openapi.module.Module
import com.intellij.openapi.project.{ ModuleListener, Project }
import com.intellij.openapi.roots._
import org.jetbrains.plugins.scala.project.settings.ScalaCompilerConfiguration
import org.jetbrains.plugins.scala.extensions.inWriteAction

object CompileToJarComponent {
  private val ProductionResourcesLibName = "compile-to-jar-resources"
  private val TestResourcesLibName = "compile-to-jar-resources-test"
  private val ResourceLibNames = Set(ProductionResourcesLibName, TestResourcesLibName)

  def getInstance(project: Project): CompileToJarComponent = {
    project.getComponent(classOf[CompileToJarComponent])
  }

  private class CompilerOutput(
    module: Module,
    getter: CompilerModuleExtension => String,
    setter: (CompilerModuleExtension, String) => Unit,
  ) {
    private val modifiableModel = ModuleRootManager.getInstance(module).getModifiableModel
    private val compilerExtension = modifiableModel.getModuleExtension(classOf[CompilerModuleExtension])

    def get: Option[String] = {
      Option(getter(compilerExtension))
    }

    def set(output: String): Unit = {
      compilerExtension.inheritCompilerOutputPath(false)
      setter(compilerExtension, output)
      inWriteAction { modifiableModel.commit() }
    }
  }

  private class ProductionOutput(module: Module)
    extends CompilerOutput(module, _.getCompilerOutputUrl, _ setCompilerOutputPath _)
  private class TestOutput(module: Module)
    extends CompilerOutput(module, _.getCompilerOutputUrlForTests, _ setCompilerOutputPathForTests _)

}

class CompileToJarComponent(project: Project) extends ProjectComponent {
  import CompileToJarComponent._

  private val connection = project.getMessageBus.connect()

  override def projectOpened(): Unit = {
    registerHookToUpdateNewModules()
    configureModulesOnStartup()
  }

  private def configureModulesOnStartup(): Unit = {
    if (compileToJar && notConfigured) {
      setupCompileToJarOutputs()
    }
  }

  private def registerHookToUpdateNewModules(): Unit = {
    connection.subscribe(ProjectTopics.MODULES, new ModuleListener {
      override def moduleAdded(project: Project, module: Module): Unit = {
        if (module.hasScala && compileToJar) {
          setupCompileToJarOutputs(module)
        }
      }
    })
  }

  private def notConfigured: Boolean = {
    project.anyScalaModule.exists { module =>
      val libraryNames = module.module.libraries.map(_.getName)
      !ResourceLibNames.forall(libraryNames.contains)
    }
  }

  override def projectClosed() {
    connection.disconnect()
  }

  private def compileToJar: Boolean = ScalaCompilerConfiguration.instanceIn(project).compileToJar

  def adjustClasspath(compileToJar: Boolean): Unit = {
    if (compileToJar) {
      setupCompileToJarOutputs()
    } else {
      setupRegularOutputs()
    }
  }

  private def setupCompileToJarOutputs(): Unit = {
    project.modulesWithScala.foreach(setupCompileToJarOutputs)
  }

  private def setupCompileToJarOutputs(module: Module): Unit = {
    val currentEntries: Set[String] = {
      val orderEntries = ModuleRootManager.getInstance(module).getOrderEntries
      orderEntries.map(_.getPresentableName)(collection.breakOut)
    }

    def configureClasspath(name: String, compilerOutput: CompilerOutput, scope: DependencyScope): Unit = {
      compilerOutput.get.foreach { url =>
        if (!url.endsWith(".jar")) {
          val jarOutput = "jar:" + url.stripPrefix("file:") + ".jar"
          val resourcesDir = url
          compilerOutput.set(jarOutput)
          if (!currentEntries.contains(name)) {
            addModuleLibrary(module, name, resourcesDir, scope)
          }
        }
      }
    }

    configureClasspath(ProductionResourcesLibName, new ProductionOutput(module), DependencyScope.COMPILE)
    configureClasspath(TestResourcesLibName, new TestOutput(module), DependencyScope.TEST)
  }

  private def setupRegularOutputs(): Unit = {
    project.modulesWithScala.foreach { module =>
      def configureOutput(compilerOutput: CompilerOutput): Unit = {
        compilerOutput.get.foreach { output =>
          if (output.endsWith(".jar")) {
            compilerOutput.set(output.stripSuffix(".jar").stripPrefix("jar:"))
          }
        }
      }

      configureOutput(new ProductionOutput(module))
      configureOutput(new TestOutput(module))
      removeOrderEntries(module, entry => ResourceLibNames.contains(entry.getPresentableName))
    }
  }

  private def addModuleLibrary(module: Module, name: String, url: String, scope: DependencyScope): Unit = {
    val classes = Collections.singletonList(url)
    val sources = Collections.emptyList[String]
    val excludedRoots = Collections.emptyList[String]
    val exported = true
    inWriteAction {
      ModuleRootModificationUtil.addModuleLibrary(module, name, classes, sources, excludedRoots, scope, exported)
    }
  }

  private def removeOrderEntries(module: Module, shouldRemove: OrderEntry => Boolean): Unit = {
    inWriteAction {
      ModuleRootModificationUtil.updateModel(module, { model =>
        val toRemove = model.getOrderEntries.filter(shouldRemove)
        toRemove.foreach(model.removeOrderEntry)
      })
    }
  }

}
