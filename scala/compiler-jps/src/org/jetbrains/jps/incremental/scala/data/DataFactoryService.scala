package org.jetbrains.jps.incremental.scala.data

import java.util.ServiceLoader

import org.jetbrains.jps.incremental.CompileContext
import org.jetbrains.jps.incremental.scala.ScalaBuilder.Log
import org.jetbrains.jps.model.JpsProject
import scala.collection.JavaConverters._

object DataFactoryService {
  def of(context: CompileContext): DataFactoryService = {
    val df = ServiceLoader.load(classOf[DataFactoryService])
    val registeredDataFactories = df.iterator().asScala.toList
    Log.info(s"Registered factories of ${classOf[DataFactoryService].getName}: $registeredDataFactories")
    val firstEnabledDataFactory = registeredDataFactories.find(_.isEnabled(context.getProjectDescriptor.getProject))
    Log.info(s"First enabled factory (if any): $firstEnabledDataFactory")
    firstEnabledDataFactory.getOrElse(DefaultDataFactoryService)
  }
}

trait DataFactoryService {
  def isEnabled(project: JpsProject): Boolean
  def getCompilerDataFactory: CompilerDataFactory
  def getCompilationDataFactory: CompilationDataFactory
}
