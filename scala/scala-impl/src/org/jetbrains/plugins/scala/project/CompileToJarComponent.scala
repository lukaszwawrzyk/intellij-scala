package org.jetbrains.plugins.scala.project

import java.util.Collections

import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots._
import org.jetbrains.plugins.scala.extensions.inWriteAction

// todo - on output changed
//  on module addded
object CompileToJarComponent {
  private val ProductionOutputJarLibName = "compile-to-jar-output"
  private val TestOutputJarLibName = "compile-to-jar-output-test"
  private val LibraryNames = Set(ProductionOutputJarLibName, TestOutputJarLibName)

  def getInstance(): CompileToJarComponent.type = this

  def adjustClasspath(project: Project, compileToJar: Boolean): Unit = {
    if (compileToJar) {
      addOutputJarsAsDependencies(project)
    } else {
      removeOutputJarDependencies(project)
    }
  }

  private def addOutputJarsAsDependencies(project: Project): Unit = {
    project.modulesWithScala.foreach { module =>
      val orderEntries = getOrderEntries(module)
      val currentUrls: Set[String] = orderEntries.flatMap(x => x.getUrls(OrderRootType.CLASSES))(collection.breakOut)

      def toMissingJar(url: String): Option[String] = Option(url).map(_ + ".jar").filterNot(currentUrls.contains)

      val compilerExtension = CompilerModuleExtension.getInstance(module)
      val missingProductionJar = toMissingJar(compilerExtension.getCompilerOutputUrl)
      val missingTestsJar = toMissingJar(compilerExtension.getCompilerOutputUrlForTests)

      inWriteAction {
        missingProductionJar.foreach(url => setModuleLibrary(module, ProductionOutputJarLibName, url, DependencyScope.COMPILE, orderEntries))
        missingTestsJar.foreach(url => setModuleLibrary(module, TestOutputJarLibName, url, DependencyScope.TEST, orderEntries))
      }
    }
  }

  private def removeOutputJarDependencies(project: Project): Unit = {
    project.modulesWithScala.foreach { module =>
      inWriteAction(removeOrderEntries(module, entry => LibraryNames.contains(entry.getPresentableName)))
    }
  }

  private def setModuleLibrary(module: Module, name: String, url: String, scope: DependencyScope, orderEntries: Seq[OrderEntry]): Unit = {
    removeModuleLibrary(module: Module, name: String, orderEntries)
    addModuleLibrary(module, name, url, scope)
  }

  private def removeModuleLibrary(module: Module, name: String, orderEntries: Seq[OrderEntry]): Unit = {
    removeOrderEntries(module, _.getPresentableName == name)
  }

  private def removeOrderEntries(module: Module, shouldRemove: OrderEntry => Boolean): Unit = {
    ModuleRootModificationUtil.updateModel(module, { model =>
      val toRemove = model.getOrderEntries.filter(shouldRemove)
      toRemove.foreach(model.removeOrderEntry)
    })
  }

  private def addModuleLibrary(module: Module, name: String, url: String, scope: DependencyScope): Unit = {
    val classes = Collections.singletonList(url)
    val sources = Collections.emptyList[String]
    val excludedRoots = Collections.emptyList[String]
    val exported = true
    ModuleRootModificationUtil.addModuleLibrary(module, name, classes, sources, excludedRoots, scope, exported)
  }

  private def getOrderEntries(module: Module): Array[OrderEntry] = {
    ModuleRootManager.getInstance(module).getOrderEntries
  }
}
