package org.jetbrains.jps.incremental.scala.data

import org.jetbrains.jps.ModuleChunk
import org.jetbrains.jps.incremental.CompileContext
import org.jetbrains.jps.incremental.ModuleLevelBuilder.ExitCode
import org.jetbrains.jps.incremental.messages.BuildMessage
import org.jetbrains.jps.incremental.messages.CompilerMessage
import org.jetbrains.jps.incremental.scala.SettingsManager
import org.jetbrains.jps.incremental.scala.data.CompilationData.javaOptionsFor
import org.jetbrains.jps.incremental.scala.data.CompilationData.scalaOptionsFor
import org.jetbrains.jps.incremental.scala.model.CompileOrder

case class CompilerConfiguration(data: CompilerData, scalacOps: Seq[String], javacOpts: Seq[String], order: CompileOrder)

object CompilerConfiguration {
  def withConfig(context: CompileContext, chunk: ModuleChunk)(op: CompilerConfiguration => ExitCode): ExitCode = {
    val commonOptions = {
      val encoding = context.getProjectDescriptor.getEncodingConfiguration.getPreferredModuleChunkEncoding(chunk)
      Option(encoding).map(Seq("-encoding", _)).getOrElse(Seq.empty)
    }
    val compilerSettings =
      SettingsManager.getProjectSettings(context.getProjectDescriptor.getProject).getCompilerSettings(chunk)

    val order = compilerSettings.getCompileOrder

    CompilerData.from(context, chunk) match {
      case Right(data) => op(CompilerConfiguration(data,
        scalacOps = commonOptions ++ scalaOptionsFor(compilerSettings, chunk),
        javacOpts = commonOptions ++ javaOptionsFor(context, chunk), order)
      )
      case Left(error) =>
        context.processMessage(new CompilerMessage("scala", BuildMessage.Kind.ERROR, error))
         ExitCode.ABORT
    }
  }
}
