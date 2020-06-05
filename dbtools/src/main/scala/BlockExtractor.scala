import java.io.{BufferedOutputStream, File, FileOutputStream, OutputStream, PrintWriter}

import bifrost.crypto.Signature25519
import bifrost.history.History
import bifrost.forging.ForgingSettings
import bifrost.modifier.block.Block
import bifrost.modifier.box.{ArbitBox, BoxSerializer}
import bifrost.nodeView.NodeViewModifier
import bifrost.utils.Logging
import com.google.common.primitives.Longs
import io.circe
import io.iohk.iodb.ByteArrayWrapper
import scopt.OParser

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

object BlockExtractor extends Logging {

  case class ExtractorConfig(config: String = "testnet-private.json",
                             output: File = new File("chain.json"),
                             errorLog: File = new File("error.json"))

  private lazy val extractorParser = {
    val builder = OParser.builder[ExtractorConfig]
    import builder._
    OParser.sequence(
      programName("extract"),
      head("Bifrost Block Extractor", "1.1"),
      opt[String]('c', "config")
          .action((x, c) => c.copy(config = x))
          .text("Node config file path"),
      opt[File]('o', "output")
          .action((x, c) => c.copy(output = x))
          .text("File to save output to")
    )
  }

  //noinspection ScalaStyle
  private def extractToJson(outStream: OutputStream, errStream: OutputStream, history: History, blockId: NodeViewModifier.ModifierId): (Int, Int, Block.BlockId) = {
    history.storage.modifierById(blockId) match {
      case Some(block) =>
        val blockJson = block.json
        val bytes = block.json.toString.getBytes
        outStream.write(bytes)
        (bytes.length, 0, block.parentId)
      case None =>
        val blockBytes = history.storage.storage.get(ByteArrayWrapper(blockId)).get.data.tail
        val parentId = blockBytes.slice(0, Block.BlockIdLength)
        log.info(s"heightOf: ${history.storage.heightOf(blockId)}")
        log.info(s"parentId: ${ByteArrayWrapper(parentId)}")
        val Array(timestamp: Long, generatorBoxLen: Long) = (0 until 2).map {
          i => Longs.fromByteArray(blockBytes.slice(Block.BlockIdLength + i * Longs.BYTES, Block.BlockIdLength + (i + 1) * Longs.BYTES))
        }.toArray
        log.info(s"timestamp: $timestamp")
        log.info(s"generatorBoxLen: $generatorBoxLen")
        val version = blockBytes.slice(Block.BlockIdLength + 2*Longs.BYTES, Block.BlockIdLength + 2*Longs.BYTES + 1).head
        log.info(s"version: $version")
        var numBytesRead = Block.BlockIdLength + Longs.BYTES * 2 + 1
        val generatorBox = BoxSerializer.parseBytes(blockBytes.slice(numBytesRead, numBytesRead + generatorBoxLen.toInt)).get.asInstanceOf[ArbitBox]
        numBytesRead += generatorBoxLen.toInt
        log.info(s"generatorBox: ${generatorBox.json}")
        if(version > 2) {
          val inflation = blockBytes.slice(numBytesRead, numBytesRead + Longs.BYTES)
          numBytesRead += Longs.BYTES
        }
        val signature = Signature25519(blockBytes.slice(numBytesRead, numBytesRead + Signature25519.SignatureSize))

        val regeneratedBlock = Block(parentId, timestamp, generatorBox, signature, Seq(), protocolVersion = version)
        log.info(s"${regeneratedBlock}")
        val bytes = regeneratedBlock.json.toString.getBytes
        outStream.write(bytes)
        errStream.write(bytes)
        (bytes.length, bytes.length, regeneratedBlock.parentId)
    }
  }

  private def extract(outputFile: File, errFile: File, settings: ForgingSettings): Unit = {
    Try(new FileOutputStream(outputFile), new FileOutputStream(errFile)) match {
      case Success((output, err)) =>
        var readBytes = ListBuffer(0, 0)
        val outStream = new BufferedOutputStream(output)
        val errStream = new BufferedOutputStream(err)

        val history = History.readOrGenerate(settings)
        var blockId = history.bestBlockId

        log.info(s"Blockchain height is ${history.height}")

        while(!(blockId sameElements settings.GenesisParentId)) {
          val bytes: (Int, Int, Block.BlockId) = extractToJson(outStream, errStream, history, blockId)

          readBytes(0) += bytes._1
          readBytes(1) += bytes._2

          blockId = bytes._3
        }

        output.close()
        outStream.close()
        err.close()
        errStream.close()
    }
  }

  private def exporter(): Unit = {
    val settings: ForgingSettings = new ForgingSettings {
      override val settingsJSON: Map[String, circe.Json] = settingsFromFile("toplnet.json")
    }
    val history: History = History.readOrGenerate(settings)

    val jsonDir = new File(s"${settings.dataDirOpt.get}/json")
    jsonDir.mkdirs()
    var tempFile = new File(s"$jsonDir/data0.json")
    var pw = new PrintWriter(tempFile)
    var block = history.bestBlock
    var height = history.height


    while (height >= 0) {
      if (height % 100000 == 0 && height / 100000 != 0) {
        pw.close()
        tempFile = new File(s"$jsonDir/data${height / 100000}.json")
        pw = new PrintWriter(tempFile)
      }

      pw.write(block.json.toString)

      println(s"${block.json}")

      block = history.modifierById(block.parentId).get
      height -= 1
    }

    pw.close()
  }

  def main(args: Array[String]): Unit = {
    OParser.parse(extractorParser, args, ExtractorConfig()) match {
      case Some(config) =>

        val settings = new ForgingSettings{
          override val settingsJSON: Map[String, circe.Json] = settingsFromFile(config.config)
        }

        extract(config.output, config.errorLog, settings)

      case _ =>
    }
  }
}
