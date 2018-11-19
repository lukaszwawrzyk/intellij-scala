package org.jetbrains.jps.incremental.scala.remote

import java.util.concurrent._

class AsynchEventGenerator(writeEvent: Event => Unit) {

  private val executors = Executors.newFixedThreadPool(1)
  private var skipProgressMessages = false

  def listener(e: Event): Unit = {
    e match {
      case _: ProgressEvent if skipProgressMessages =>
        return
      case _: StartProcessingOutputEvent =>
        skipProgressMessages = true
      case _: StopProcessingOutputEvent =>
        skipProgressMessages = false
      case _ =>
    }
    executors.submit(new Runnable {
      override def run(): Unit = {
        // if compilcation is done no need to process progress events anymore...
        if (!skipProgressMessages || !e.isInstanceOf[ProgressEvent]) {
          writeEvent(e)
        }
      }
    })
  }

  def complete(): Unit = {
    executors.shutdown()
    executors.awaitTermination(20, TimeUnit.MINUTES)
  }
}