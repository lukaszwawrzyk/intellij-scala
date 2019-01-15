package org.jetbrains.jps.incremental.scala

import _root_.java.io._
import java.net.InetAddress
import java.util.ServiceLoader

import com.intellij.openapi.application.PathManager
import com.intellij.openapi.diagnostic.{Logger => JpsLogger}
import org.jetbrains.jps.ModuleChunk
import com.intellij.openapi.util.io.FileUtil
import org.jetbrains.jps.builders.java.JavaBuilderUtil
import org.jetbrains.jps.incremental._
import org.jetbrains.jps.incremental.messages.ProgressMessage
import org.jetbrains.jps.incremental.scala.data.CompilerConfiguration
import org.jetbrains.jps.incremental.scala.data.{CompilationData, CompilerData, SbtData}
import org.jetbrains.jps.incremental.scala.data.DataFactoryService
import org.jetbrains.jps.incremental.scala.data.DefaultDataFactoryService
import org.jetbrains.jps.incremental.scala.local.LocalServer
import org.jetbrains.jps.incremental.scala.model.{GlobalSettings, ProjectSettings}
import org.jetbrains.jps.incremental.scala.remote.RemoteServer
import org.jetbrains.jps.model.module.JpsModule

import _root_.scala.collection.JavaConverters._

/**
 * Nikolay.Tropin
 * 11/19/13
 */

object ScalaBuilder {

  def compile(context: CompileContext,
              chunk: ModuleChunk,
              compilerConfig: CompilerConfiguration,
              sources: Seq[File],
              allSources: Seq[File],
              modules: Set[JpsModule],
              client: Client): Either[String, ModuleLevelBuilder.ExitCode] = {

    context.processMessage(new ProgressMessage("Reading compilation settings..."))

    for {
      sbtData <-  sbtData
      dataFactory = dataFactoryOf(context)
      compilerData <- dataFactory.getCompilerDataFactory.from(context, chunk)
      compilationData <- dataFactory.getCompilationDataFactory.from(sources, allSources, context, chunk, compilerConfig)
    }
    yield {
      scalaLibraryWarning(modules, compilationData, client)

      val server = getServer(context)
      server.compile(sbtData, compilerData, compilationData, client)
    }
  }

  def checkIncrementalTypeChange(context: CompileContext): Unit = {
    def storageFile: Option[File] = {
      val projectDir = context.getProjectDescriptor.dataManager.getDataPaths.getDataStorageRoot
      if (projectDir != null)
        Some(new File(projectDir, "incrementalType.dat"))
      else None
    }

    def getPreviousIncrementalType: Option[IncrementalityType] = {
      storageFile.filter(_.exists).flatMap { file =>
        val result = using(new DataInputStream(new BufferedInputStream(new FileInputStream(file)))) { in =>
          try {
            Some(IncrementalityType.valueOf(in.readUTF()))
          } catch {
            case _: IOException | _: IllegalArgumentException | _: NullPointerException => None
          }
        }
        if (result.isEmpty) file.delete()
        result
      }
    }

    def setPreviousIncrementalType(incrType: IncrementalityType) {
      storageFile.foreach { file =>
        val parentDir = file.getParentFile
        if (!parentDir.exists()) parentDir.mkdirs()
        using(new DataOutputStream(new BufferedOutputStream(new FileOutputStream(file)))) {
          _.writeUTF(incrType.name)
        }
      }
    }

    def cleanCaches() {
      context.getProjectDescriptor.setFSCache(FSCache.NO_CACHE)
      try {
        val directory = context.getProjectDescriptor.dataManager.getDataPaths.getDataStorageRoot
        FileUtil.delete(directory)
      }
      catch {
        case e: Exception => throw new IOException("Can not delete project system directory: \n" + e.getMessage)
      }
    }

    val settings = projectSettings(context)
    val previousIncrementalType = getPreviousIncrementalType
    val incrType = settings.getIncrementalityType
    previousIncrementalType match {
      case _ if JavaBuilderUtil.isForcedRecompilationAllJavaModules(context) => //isRebiuld
        setPreviousIncrementalType(incrType)
      case None =>
      //        ScalaBuilderDelegate.Log.info("scala: cannot find type of the previous incremental compiler, full rebuild may be required")
      case Some(`incrType`) => //same incremental type, nothing to be done
      case Some(_) if isMakeProject(context) =>
        if (ScalaBuilder.isScalaProject(context.getProjectDescriptor.getProject)) {
          cleanCaches()
          setPreviousIncrementalType(incrType)
          context.processMessage(new CompilerMessage("scala", BuildMessage.Kind.WARNING,
            "type of incremental compiler has been changed, full rebuild..."))
        }
      case Some(_) =>
        if (ScalaBuilder.isScalaProject(context.getProjectDescriptor.getProject)) {
          throw new ProjectBuildException("scala: type of incremental compiler has been changed, full rebuild is required")
        }
        server.compile(sbtData, compilerConfig.data, compilationData, client)
    }
  }

  private def dataFactoryOf(context: CompileContext): DataFactoryService = {
    val df = ServiceLoader.load(classOf[DataFactoryService])
    val registeredDataFactories = df.iterator().asScala.toList
    Log.info(s"Registered factories of ${classOf[DataFactoryService].getName}: $registeredDataFactories")
    val firstEnabledDataFactory = registeredDataFactories.find(_.isEnabled(context.getProjectDescriptor.getProject))
    Log.info(s"First enabled factory (if any): $firstEnabledDataFactory")
    firstEnabledDataFactory.getOrElse(DefaultDataFactoryService)
  }

  def hasBuildModules(chunk: ModuleChunk): Boolean = {
    chunk.getModules.asScala.exists(_.getName.endsWith("-build")) // gen-idea doesn't use the sbt module type
  }

  def isMakeProject(context: CompileContext): Boolean = JavaBuilderUtil.isCompileJavaIncrementally(context) && {
    for {
      chunk <- context.getProjectDescriptor.getBuildTargetIndex.getSortedTargetChunks(context).asScala
      target <- chunk.getTargets.asScala
    } {
      if (!context.getScope.isAffected(target)) return false
    }
    true
  }

  def projectSettings(context: CompileContext): ProjectSettings = SettingsManager.getProjectSettings(context.getProjectDescriptor.getProject)

  val Log: JpsLogger = JpsLogger.getInstance(ScalaBuilder.getClass.getName)

  // Cached local localServer
  private var cachedServer: Option[Server] = None

  private val lock = new Object()

  def localServer: Server = {
    lock.synchronized {
      val server = cachedServer.getOrElse(new LocalServer())
      cachedServer = Some(server)
      server
    }
  }

  private def cleanLocalServerCache() {
    lock.synchronized {
      cachedServer = None
    }
  }

  private lazy val sbtData = {
    val classLoader = getClass.getClassLoader
    val pluginRoot = new File(PathManager.getJarPathForClass(getClass)).getParentFile
    val javaClassVersion = System.getProperty("java.class.version")

    SbtData.from(classLoader, pluginRoot, javaClassVersion)
  }

  private def scalaLibraryWarning(modules: Set[JpsModule], compilationData: CompilationData, client: Client) {
    val hasScalaLibrary = compilationData.classpath.exists(_.getName.startsWith("scala-library"))

    val hasScalaSources = compilationData.sources.exists(_.getName.endsWith(".scala"))

    if (!hasScalaLibrary && hasScalaSources) {
      val names = modules.map(_.getName).mkString(", ")
      client.warning("No 'scala-library*.jar' in module dependencies [%s]".format(names))
    }
  }

  private def getServer(context: CompileContext): Server = {
    if (isCompileServerEnabled(context)) {
      cleanLocalServerCache()
      new RemoteServer(InetAddress.getByName(null), globalSettings(context).getCompileServerPort)
    } else {
      localServer
    }
  }

  def isCompileServerEnabled(context: CompileContext): Boolean =
    globalSettings(context).isCompileServerEnabled && isCompilationFromIDEA(context)

  //hack to not run compile server on teamcity; is there a better way?
  private def isCompilationFromIDEA(context: CompileContext): Boolean =
    JavaBuilderUtil.CONSTANT_SEARCH_SERVICE.get(context) != null

  private def globalSettings(context: CompileContext): GlobalSettings =
    SettingsManager.getGlobalSettings(context.getProjectDescriptor.getModel.getGlobal)
}
