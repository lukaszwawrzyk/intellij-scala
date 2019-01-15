package org.jetbrains.jps.incremental.scala.remote

import java.io._
import java.net.InetSocketAddress
import java.net.{ InetAddress, Socket }
import java.nio.channels.SocketChannel
import java.util.concurrent.Semaphore

import com.martiansoftware.nailgun.NGConstants
import org.jetbrains.jps.incremental.messages.BuildMessage.Kind
import org.jetbrains.jps.incremental.scala._

object RemoteResourceOwner {
  val parallelResourcesLimitPropertyName = "org.jetbrains.jps.parallelResourcesLimit"
  val defaultParallelResourcesLimit = 20
  val parallelResourcesLimit = Option(System.getProperty(parallelResourcesLimitPropertyName))
  private val socketSemaphore = new Semaphore(parallelResourcesLimit.fold(defaultParallelResourcesLimit)(_.toInt))

  def withConstrainedResources(block: => Unit): Unit = {
    socketSemaphore.acquire(1)
    try block finally socketSemaphore.release(1)
  }
}

/**
 * @author Pavel Fatin
 * @author Dmitry Naydanov
 */
trait RemoteResourceOwner {
  import RemoteResourceOwner._

  protected val address: InetAddress
  protected val port: Int

  protected val currentDirectory = System.getProperty("user.dir")
  protected val serverAlias = "compile-server"

  def send(command: String, arguments: Seq[String], client: Client): Unit = {
    val channel = SocketChannel.open(new InetSocketAddress(address, port))
    withConstrainedResources {
      using(channel.socket()) { socket =>
        using(outputStream(socket)) { output =>
          createChunks(command, arguments).foreach(_.writeTo(output))
          output.flush()
          if (client != null) {
            using(inputStream(socket))(input => handle(input, client))
          }
        }
      }
    }
  }

  private def inputStream(socket: Socket) = {
    new DataInputStream(new BufferedInputStream(socket.getInputStream))
  }

  private def outputStream(socket: Socket) = {
    new DataOutputStream(new BufferedOutputStream(socket.getOutputStream))
  }

  protected def handle(input: DataInputStream, client: Client) {
    val processor = new ClientEventProcessor(client)

    while (!client.isCanceled) {
      Chunk.readFrom(input) match {
        case Chunk(NGConstants.CHUNKTYPE_EXIT, _) =>
          return
        case Chunk(NGConstants.CHUNKTYPE_STDOUT, data) =>
          try {
            val event = Protocol.deserializeEvent(data)
            processor.process(event)
          } catch {
            case e: Exception =>
              val chars = {
                val s = new String(data)
                if (s.length > 50) s.substring(0, 50) + "..." else s
              }
              client.error("Unable to read an event from: " + chars)
              client.trace(e)
          }
        // Main server class redirects all (unexpected) stdout data to stderr.
        // In theory, there should be no such data at all, however, in practice,
        // sbt "leaks" some messages into console (e.g. for "explain type errors" option).
        // Report such output not as errors, but as warnings (to continue make process).
        case Chunk(NGConstants.CHUNKTYPE_STDERR, data) =>
          client.warning(Protocol.fromBytes(data))
        case Chunk(kind, data) =>
          client.error("Unexpected server output: " + data)
      }
    }
  }

  protected def createChunks(command: String, args: Seq[String]): Seq[Chunk] = {
    val serializedArgs = Protocol.serializeArgs(args)
    val argsChunks = Chunk(NGConstants.CHUNKTYPE_ARGUMENT.toChar, serializedArgs)
    val cwdChunk = Chunk(NGConstants.CHUNKTYPE_WORKINGDIRECTORY.toChar, Protocol.toBytes(currentDirectory))
    val commandChunk = Chunk(NGConstants.CHUNKTYPE_COMMAND.toChar, Protocol.toBytes(command))
    Seq(argsChunks, cwdChunk, commandChunk)
  }

}

case class Chunk(kind: Chunk.Kind, data: Array[Byte]) {
  def writeTo(output: DataOutputStream) {
    output.writeInt(data.length)
    output.writeByte(kind.toByte)
    output.write(data)
  }
}

object Chunk {
  type Kind = Char

  def readFrom(input: DataInputStream): Chunk = {
    val size = input.readInt()
    val kind = input.readByte().toChar
    val data = {
      val buffer = new Array[Byte](size)
      input.readFully(buffer)
      buffer
    }
    Chunk(kind, data)
  }
}