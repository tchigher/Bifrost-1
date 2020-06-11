import java.io.{BufferedOutputStream, File, FileOutputStream, OutputStream}
import java.text.SimpleDateFormat
import java.util.{Date, TimeZone}

import bifrost.crypto.Signature25519
import bifrost.forging.ForgingSettings
import bifrost.history.History
import bifrost.modifier.block.Block
import bifrost.modifier.box.{ArbitBox, BoxSerializer}
import bifrost.modifier.transaction.bifrostTransaction._
import bifrost.nodeView.NodeViewModifier
import bifrost.utils.Logging
import com.google.common.primitives.Longs
import io.circe
import io.circe.syntax._
import io.iohk.iodb.ByteArrayWrapper
import scopt.OParser
import scorex.crypto.encode.Base58

import scala.collection.mutable.ListBuffer
import scala.util.{Success, Try}

object BlockExtractor extends Logging {

  case class ExtractorConfig(config: String = "testnet-private.json",
                             output: File = new File("chain.json"),
                             errorLog: File = new File("error.json"),
                             txsLog: File = new File("txs.json"))

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

  private def formatDate(timestamp: Long) = {
    val date = new Date(timestamp)
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ") //2020-06-07T07:27:10.080Z
    dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"))
    dateFormat.format(date)
  }

  private def txsToJson(assetStream: OutputStream, transferStream: OutputStream, atStream: OutputStream, txs: Seq[Transaction]) = {
    txs.foreach {
      case mat: mAssetCreation =>
        assetStream.write(mat.json.deepMerge(
          Map("txType" -> "AssetCreation",
          "timestamp" -> formatDate(mat.timestamp),
          "issuer" -> Base58.encode(mat.hub.bytes)).asJson).toString.getBytes ++ ",\n".getBytes)
      case p: mPolyTransfer =>
      transferStream.write(p.json.deepMerge(
        Map("txType" -> "PolyTransfer",
        "timestamp" -> formatDate(p.timestamp)).asJson).toString.getBytes ++ ",\n".getBytes)
      case ar: mArbitTransfer =>
      transferStream.write(ar.json.deepMerge(
        Map("txType" -> "ArbitTransfer",
        "timestamp" -> formatDate(ar.timestamp)).asJson).toString.getBytes ++ ",\n".getBytes)
      case at: mAssetTransfer =>
      atStream.write(at.json.deepMerge(
        Map("txType" -> "AssetTransfer",
          "timestamp" -> formatDate(at.timestamp),
          "issuer" -> Base58.encode(at.hub.bytes)).asJson).toString.getBytes ++ ",\n".getBytes)
    }
  }

  //noinspection ScalaStyle
  private def extractToJson(outStream: OutputStream, errStream: OutputStream, assetStream: OutputStream, transferStream: OutputStream, assetTransferStream: OutputStream, history: History, blockId: NodeViewModifier.ModifierId): (Int, Int, Block.BlockId) = {
    history.storage.modifierById(blockId) match {
      case Some(block) =>
        val blockJson = block.json
        val bytes = block.json.deepMerge(Map(
          "blockNumber"-> history.storage.heightOf(blockId).get.toString,
          "blockDifficulty" -> history.storage.difficultyOf(blockId).get.toString,
          "timestamp" -> formatDate(block.timestamp)).asJson).toString.getBytes ++ ",\n".getBytes
        /*log.info(s"${
          block.json.deepMerge(Map(
            "blockNumber"-> history.storage.heightOf(blockId).get.toString,
            "blockDifficulty" -> history.storage.difficultyOf(blockId).get.toString).asJson).toString
        }")*/

        txsToJson(assetStream, transferStream, assetTransferStream, block.txs)
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
        val bytes = regeneratedBlock.json.toString.getBytes ++ ",\n".getBytes
        val rawBytes = regeneratedBlock.json.deepMerge(Map(
          "blockNumber" -> history.storage.heightOf(blockId).get.toString,
          "blockDifficulty" -> history.storage.difficultyOf(blockId).get.toString,
          "rawBytes" -> Base58.encode(regeneratedBlock.bytes)).asJson).toString.getBytes ++ ",\n".getBytes
        outStream.write(bytes)
        errStream.write(rawBytes)
        (bytes.length, rawBytes.length, regeneratedBlock.parentId)
    }
  }

  private def extract(outputFile: File, errFile: File, txFile: File, settings: ForgingSettings): Unit = {
    Try(new FileOutputStream(outputFile), new FileOutputStream(errFile), new FileOutputStream(txFile)) match {
      case Success((output, err, txs)) =>
        val readBytes = ListBuffer(0, 0)
        val outStream = new BufferedOutputStream(output)
        val errStream = new BufferedOutputStream(err)
        val assetStream = new BufferedOutputStream(new FileOutputStream(new File("assetCreations.json")))
        val transferStream = new BufferedOutputStream(new FileOutputStream(new File("transfers.json")))
        val assetTransferStream = new BufferedOutputStream(new FileOutputStream(new File("assetTransfers.json")))

        val history = History.readOrGenerate(settings)
        var blockId = history.bestBlockId
        val height = history.height
        val start = System.nanoTime()

        log.info(s"Blockchain height is ${height}")

        while(!(blockId sameElements settings.GenesisParentId)) {
          val bytes: (Int, Int, Block.BlockId) = extractToJson(outStream, errStream, assetStream, transferStream, assetTransferStream, history, blockId)

          readBytes(0) += bytes._1
          readBytes(1) += bytes._2

          blockId = bytes._3
        }

        val duration = (System.nanoTime() - start) / 1e9d

        log.info(s"Extracted $height blocks in $duration seconds")

        outStream.flush()
        errStream.flush()
        assetStream.flush()
        transferStream.flush()
        outStream.close()
        errStream.close()
        assetStream.close()
        transferStream.close()
    }
  }

  def main(args: Array[String]): Unit = {
    OParser.parse(extractorParser, args, ExtractorConfig()) match {
      case Some(config) =>

        val settings = new ForgingSettings{
          override val settingsJSON: Map[String, circe.Json] = settingsFromFile(config.config)
        }

        extract(config.output, config.errorLog, config.txsLog, settings)

      case _ =>
    }
  }
}
