import bifrost.history.History
import bifrost.state.State
import bifrost.modifier.ModifierId
import bifrost.modifier.block.{Block, BlockSerializer}
import bifrost.settings.{AppSettings, StartupOpts}
import bifrost.utils.Logging
import scorex.crypto.encode.Base58
import io.iohk.iodb.ByteArrayWrapper

import scala.collection.mutable.ArrayBuffer
import scala.reflect.io.Path
import scala.util.{Failure, Success, Try}
import java.io.{FileWriter, PrintWriter}

import scala.io.Source

object DBMigration extends Logging {

  def main(args: Array[String]): Unit = {
    val oldSettingsFilename = "dbtools/src/main/resources/oldData.conf"
    val oldSettings: AppSettings = AppSettings.read(StartupOpts(Some(oldSettingsFilename), None))
    val newSettingsFilename = "dbtools/src/main/resources/newData.conf"
    val newSettings: AppSettings = AppSettings.read(StartupOpts(Some(newSettingsFilename), None))
    val idFileName: String = ".bifrost/blockIds/bids.txt"
    val statusFile: String = ".bifrost/blockIds/status.txt"
    val newDataDir: String = ".bifrost/new-data"
    val startNew: Boolean = true
    val historyOrState: Boolean = true // history: true | state: false

    /* getIds(oldSettings) */

    if (!new java.io.File(statusFile).exists()){
      if (!new java.io.File(newDataDir).exists())
        migrate(oldSettings, newSettings, idFileName, statusFile, newDataDir, startNew, historyOrState, 0)
      else
        log.warn(s"${Console.YELLOW} new-data exists but no status.txt found in /blockIds/, delete new-data!!!!!!!!!${Console.RESET}")
    } else {
      val statusSource = Source.fromFile(statusFile)
      val status = statusSource.getLines().toList

      if (status.size <= 0) {
        require(!new java.io.File(newDataDir).exists())

        migrate(oldSettings, newSettings, idFileName, statusFile, newDataDir, startNew, historyOrState, 0)
      } else {
        if (status.head == "running") {
          log.warn(s"${Console.YELLOW}Last run didn't finish!!!!!!!! Remove new-data and clear status.txt${Console.RESET}")
        } else if (status.head == "finished") {
          var prevHeight = -1
          if (startNew) prevHeight = 0
          else prevHeight = status(1).toInt

          migrate(oldSettings, newSettings, idFileName, statusFile, newDataDir, startNew, historyOrState, prevHeight)
        }
      }
    }
  }

  // noinspection ScalaStyle:off method.maxLength
  private def migrate(oldSettings: AppSettings, newSettings: AppSettings, idFileName: String, statusFile: String,
                      newDataDir: String, startNew: Boolean, historyOrState: Boolean, prevHeight: Int): Unit = {
    if (startNew) {
      val newDataPath: Path = Path(newDataDir)
      Try(newDataPath.deleteRecursively())
    }

    log.debug(s"-----------height from status file:$prevHeight")

    val idsFile = Source.fromFile(idFileName)
    val oldHistory: History = History.readOrGenerate(oldSettings)
    val newHistory: History = History.readOrGenerate(newSettings)
    val newState: State = State.readOrGenerate(newSettings, callFromGenesis = true, newHistory)
    var parentBlockId: ModifierId = newHistory.bestBlockId

    var height =
      if(historyOrState) newHistory.height.toInt
      else prevHeight

    val initialHeight: Int = height
    val start = System.nanoTime()

    val statusStart = new PrintWriter(new FileWriter(statusFile, false))
    statusStart.write("running")
    statusStart.close()

    log.debug(s"initial height is $height")

    val targetTime: Long = 1600
    val targetNum: Int = 1000
    idsFile.getLines
      .drop(initialHeight)
      // .take(targetNum)
      // .takeWhile(_ => runtimer(start, targetTime))
      .foreach{ line =>
        val bid: Array[Byte] = Base58.decode(line).get
        height += 1

        val currentBlock: Block = oldHistory.storage.storage.get(ByteArrayWrapper(bid)).map { bw =>
          val bytes = bw.data
          BlockSerializer.decode(bytes.tail).get
        }.get.copy(parentId = parentBlockId)

        val currentDifficulty: Long = oldHistory.storage.difficultyOf(ModifierId(bid)).get

        Try {
          if(historyOrState) newHistory.storage.update(currentBlock, currentDifficulty, isBest = true)
          else newState.applyModifierOnState(currentBlock)
        } match {
          case Success(_) => log.debug(s"-----------------------------------Height:$height----Diff:$currentDifficulty")
          case Failure(_) => log.warn(s"Failed to append and apply block $height !!!!!!!")
        }

        parentBlockId = currentBlock.id
        //        log.debug(s"^^^^^^^^^^$height-----------\n${currentBlock.json}")
      }

    val statusEnd = new PrintWriter(new FileWriter(statusFile, false))
    statusEnd.write(s"finished\n$height")
    statusEnd.close()
    idsFile.close()
    val duration = (System.nanoTime() - start) / 1e9d
    log.debug(s"Finished at height:$height in $duration seconds")
  }

  private def runtimer(startTime: Long, duration: Long): Boolean = {
    if ((System.nanoTime() - startTime) / 1e9d >= duration) false
    else true
  }

  private def getIds(oldSettings: AppSettings, idFileName: String): Unit = {

    /* If new-data data exists, remove before creating new-data */

    val writer = new PrintWriter(new FileWriter(idFileName, false))
    var idList: ArrayBuffer[String] = ArrayBuffer[String]()
    val oldHistory: History = History.readOrGenerate(oldSettings)
    var blockId: ModifierId = oldHistory.bestBlockId
    var height: Long = oldHistory.height
    val start = System.nanoTime()

    log.info(s"Blockchain is at height $height, blockId: ${Base58.encode(blockId.hashBytes)}")

    while(!(blockId.hashBytes sameElements History.GenesisParentId)) {
      val currentBlock: Block = oldHistory.storage.storage.get(ByteArrayWrapper(blockId.hashBytes)).map { bw =>
        val bytes = bw.data
        BlockSerializer.decode(bytes.tail).get
      }.get

      idList += Base58.encode(blockId.hashBytes)

      log.info(s"----$height----${Base58.encode(blockId.hashBytes)}----${Base58.encode(currentBlock.serializedId)}" +
        s"----BlockParentId:${Base58.encode(currentBlock.parentId.hashBytes)}")
      blockId = currentBlock.parentId

      height -= 1
    }

    for (bid <- idList.reverse) writer.write(s"$bid\n")
    writer.close()
    val duration = (System.nanoTime() - start) / 1e9d
    log.info(s"Found all blockIds in $duration seconds")
  }
}
