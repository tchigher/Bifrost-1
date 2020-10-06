import bifrost.history.History
import bifrost.state.State
import bifrost.modifier.ModifierId
import bifrost.modifier.block.{Block, BlockSerializer}
import bifrost.settings.{AppSettings, StartupOpts}
import bifrost.utils.Logging
import com.google.common.primitives.Longs
import scorex.crypto.encode.Base58
import io.iohk.iodb.ByteArrayWrapper

import scala.collection.mutable.ArrayBuffer
import scala.reflect.io.Path
import scala.util.{Failure, Success, Try}
import java.io.File
import java.io.PrintWriter

import scala.io.Source

object DBMigration extends Logging {

  private def getIds(oldSettings: AppSettings): Unit = {

    /* If new-data data exists, remove before creating new-data */
    val path: Path = Path(".bifrost/blockIds/bids.txt")
    Try(path.deleteRecursively())

    val writer = new PrintWriter(new File(".bifrost/blockIds/bids.txt"))
    var idList: ArrayBuffer[String] = ArrayBuffer[String]()
    val oldHistory: History = History.readOrGenerate(oldSettings)
    var blockId: ModifierId = oldHistory.bestBlockId
    var height: Long = oldHistory.height
    val start = System.nanoTime()

    log.info(s"Blockchain height is ${height}, blockId: ${Base58.encode(blockId.hashBytes)}")

    while(!(blockId.hashBytes sameElements History.GenesisParentId)) {
      val currentBlock: Block = oldHistory.storage.storage.get(ByteArrayWrapper(blockId.hashBytes)).map { bw =>
        val bytes = bw.data
        BlockSerializer.decode(bytes.tail).get
      }.get

      idList += Base58.encode(blockId.hashBytes)

      log.info(s"----${height}----${Base58.encode(blockId.hashBytes)}----${Base58.encode(currentBlock.serializedId)}" +
        s"----BlockParentId:${Base58.encode(currentBlock.parentId.hashBytes)}")
      blockId = currentBlock.parentId

      height = height - 1
    }

    for (bid <- idList.reverse) writer.write(s"$bid\n")
    writer.close()
    val duration = (System.nanoTime() - start) / 1e9d
    log.info(s"Found all blockIds in $duration seconds")
  }

  private def runtimer(starttime: Long, duration: Long): Boolean = {
    if ((System.nanoTime() - starttime) / 1e9d >= duration) false
    else true
  }

  private def migrate(oldSettings: AppSettings, newSettings: AppSettings, startNew: Boolean): Unit = {

    if (startNew) {
      /* If new-data data exists, remove before creating new-data */
      val path: Path = Path(".bifrost/new-data")
      Try(path.deleteRecursively())
    }

    val idsFile = Source.fromFile(".bifrost/blockIds/bids.txt")
    val oldHistory: History = History.readOrGenerate(oldSettings)
    val newHistory: History = History.readOrGenerate(newSettings)
    val newState: State = State.readOrGenerate(newSettings, callFromGenesis = true, newHistory)
    var parentBlockId: ModifierId = newHistory.bestBlockId
    var height = newHistory.height.toInt

    log.debug(s"initial height is $height -- ${Base58.encode(newHistory.bestBlockId.hashBytes)}")

    val start = System.nanoTime()

    val targetTime: Long = 16500
    val targetNum: Int = 10
    idsFile.getLines
      .drop(newHistory.height.toInt)
      .take(targetNum)
//      .takeWhile(_ => runtimer(start, targetTime))
      .foreach{ line =>

        val bid: Array[Byte] = Base58.decode(line).get
        height += 1

        val currentBlock: Block = oldHistory.storage.storage.get(ByteArrayWrapper(bid)).map { bw =>
          val bytes = bw.data
          BlockSerializer.decode(bytes.tail).get
        }.get.copy(parentId = parentBlockId)

        val currentDifficulty: Long = oldHistory.storage.difficultyOf(ModifierId(bid)).get

        Try {
          newHistory.storage.update(currentBlock, currentDifficulty, isBest = true)
          newState.applyModifierOnState(currentBlock)
        } match {
          case Success(_) => log.debug("-------------------------------------------------------------------"+
            s"----------------------------------------Height:$height----Diff:$currentDifficulty")
          case Failure(_) => log.warn(s"Failed to append and apply block $height !!!!!!!")
        }

        parentBlockId = currentBlock.id

//        log.debug(s"^^^^^^^^^^$height-----------\n${currentBlock.json}")
    }

    idsFile.close()
    val duration = (System.nanoTime() - start) / 1e9d

    log.debug(s"Finished at height:$height in $duration seconds")
  }

  def main(args: Array[String]): Unit = {

    val oldSettingsFilename = "dbtools/src/main/resources/oldData.conf"
    val oldSettings: AppSettings = AppSettings.read(StartupOpts(Some(oldSettingsFilename), None))

    val newSettingsFilename = "dbtools/src/main/resources/newData.conf"
    val newSettings: AppSettings = AppSettings.read(StartupOpts(Some(newSettingsFilename), None))

//    getIds(oldSettings)
    migrate(oldSettings, newSettings, false)
  }
}
