package org.jetbrains.jps.incremental.scala

import org.jetbrains.jps.incremental.scala.data.ZincData
import org.jetbrains.jps.service.JpsServiceManager

import scala.collection.JavaConverters._

abstract class ZincDataService {
  def transform(zincData: ZincData): ZincData
}

object ZincDataService {
  private val services: Seq[ZincDataService] = JpsServiceManager.getInstance
    .getExtensions(classOf[ZincDataService]).asScala.toList

  def transform(zincData: ZincData): ZincData = {
    services.foldLeft(zincData)((data, service) => service.transform(data))
  }
}