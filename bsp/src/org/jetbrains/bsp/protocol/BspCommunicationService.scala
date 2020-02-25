package org.jetbrains.bsp.protocol

import java.io.File
import java.net.URI
import java.nio.file._
import java.util.concurrent.CompletableFuture
import java.util.concurrent.TimeUnit

import com.intellij.ProjectTopics
import com.intellij.openapi.module.Module
import com.intellij.openapi.Disposable
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.components.ServiceManager
import com.intellij.openapi.externalSystem.util.ExternalSystemApiUtil
import com.intellij.openapi.project.ModuleListener
import com.intellij.openapi.project.Project
import com.intellij.openapi.project.ProjectManager
import com.intellij.openapi.project.ProjectManagerListener
import com.intellij.openapi.project.ProjectUtil
import com.intellij.openapi.util.Disposer
import com.intellij.util.concurrency.AppExecutorUtil
import org.jetbrains.bsp.settings.BspExecutionSettings

import scala.collection.mutable
import scala.concurrent.duration._
import scala.util.Success
import scala.util.Try

class BspCommunicationService extends Disposable {

  import BspCommunicationService.projectPath

  { // init
    val app = ApplicationManager.getApplication
    Disposer.register(app, this)

    val bus = ApplicationManager.getApplication.getMessageBus.connect()
    bus.subscribe(ProjectManager.TOPIC, MyProjectListener)
  }

  private val timeout = 10.minutes
  private val cleanerPause = 10.seconds

  private val comms = mutable.Map[URI, BspCommunication]()

  private val executorService = AppExecutorUtil.getAppScheduledExecutorService

  private val commCleaner = executorService
    .scheduleWithFixedDelay(() => closeIdleSessions(), cleanerPause.toMillis, cleanerPause.toMillis, TimeUnit.MILLISECONDS)

  private def closeIdleSessions(): Unit = {
    val now = System.currentTimeMillis()
    comms.values.foreach { comm =>
      if (comm.isIdle(now, timeout))
        comm.closeSession()
    }
  }

  def communicate(base: File): BspCommunication =
    comms.getOrElseUpdate(
      base.getCanonicalFile.toURI,
      {
        val comm = new BspCommunication(base, executionSettings(base))
        Disposer.register(this, comm)
        comm
      }
    )

  def communicate(implicit project: Project): BspCommunication =
    projectPath.map(new File(_))
      .map(communicate)
      .orNull // TODO

  def closeCommunication(base: File): Try[Unit] =
    comms.get(base.getCanonicalFile.toURI)
      .toRight(new NoSuchElementException)
      .toTry
      .flatMap(_.closeSession())

  def closeAll: Try[Unit] =
    comms.values
      .map(_.closeSession())
      .foldLeft(Success(Unit): Try[Unit])(_.orElse(_))

  private def executionSettings(base: File): BspExecutionSettings =
    BspExecutionSettings.executionSettingsFor(base)

  override def dispose(): Unit = {
    comms.values.foreach(_.closeSession())
    commCleaner.cancel(true)
  }

  private object MyProjectListener extends ProjectManagerListener {
    /*override def projectOpened(project: Project): Unit = {
      val conn = project.getMessageBus.connect()
      conn.subscribe(ProjectTopics.MODULES, new ModuleListener {
        var initialized = false

        override def moduleAdded(project: Project, module: Module): Unit = {
          if (!initialized)
            synchronized {
              if (!initialized) {
                initialized = true
                val path = new File(ExternalSystemApiUtil.getExternalProjectPath(module))
                communicate(path).run((_, _) => CompletableFuture.completedFuture(()), _ => (), _ => ())
                conn.disconnect()
              }
            }
        }
      }
      )
    }
*/
    override def projectClosed(project: Project): Unit = for {
      path <- projectPath(project)
      uri = Paths.get(path).toUri
      session <- comms.get(uri)
    } session.closeSession()
  }

}

object BspCommunicationService {

  def getInstance: BspCommunicationService =
    ServiceManager.getService(classOf[BspCommunicationService])

  private def projectPath(implicit project: Project): Option[String] =
    Option(ProjectUtil.guessProjectDir(project))
      .map(_.getCanonicalPath)
}