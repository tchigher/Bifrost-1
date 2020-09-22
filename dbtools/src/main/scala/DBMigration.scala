import bifrost.history.History
import bifrost.state.State
import bifrost.modifier.ModifierId
import bifrost.modifier.block.{Block, BlockSerializer}
import bifrost.settings.{AppSettings, StartupOpts}
import bifrost.utils.Logging
import scorex.crypto.encode.Base58
import io.iohk.iodb.ByteArrayWrapper

import scala.collection.mutable.ArrayBuffer

object DBMigration extends Logging {

  private def migrate(oldSettings: AppSettings, newSettings: AppSettings): Unit = {
    var listBlockId: ArrayBuffer[Array[Byte]] = ArrayBuffer[Array[Byte]]()
    val oldHistory: History = History.readOrGenerate(oldSettings)
    val newHistory: History = History.readOrGenerate(newSettings)
//    val newState: State = State.readOrGenerate(newSettings, callFromGenesis = true, newHistory)
    var blockId: ModifierId = oldHistory.bestBlockId
    var height: Long = oldHistory.height
    var start = System.nanoTime()

    println(s"---------------${oldSettings.dataDir}")

    log.info(s"Blockchain height is ${height}, blockId: ${Base58.encode(blockId.hashBytes)}")

    while(!(blockId.hashBytes sameElements History.GenesisParentId) && height > 1059465) {
      val currentBlock: Block = oldHistory.storage.storage.get(ByteArrayWrapper(blockId.hashBytes)).map { bw =>
        val bytes = bw.data
        BlockSerializer.decode(bytes.tail).get
      }.get

      listBlockId += blockId.hashBytes
      println(s"------${height}---------${Base58.encode(blockId.hashBytes)}------${Base58.encode(currentBlock.serializedId)}")
      blockId = currentBlock.parentId

      height = height - 1
    }

    val duration = (System.nanoTime() - start) / 1e9d

    log.info(s"Found ${listBlockId.size} blockIds in $duration seconds")

    start = System.nanoTime()

    for (bid <- listBlockId) {
      val currentBlock: Block = oldHistory.storage.storage.get(ByteArrayWrapper(bid)).map { bw =>
        val bytes = bw.data
        BlockSerializer.decode(bytes.tail).get
      }.get
      println(s"-------${Base58.encode(bid)}-----${Base58.encode((currentBlock.serializedId))}")
//      newHistory.append(currentBlock)
//      state.applyModifier(currentBlock)
    }

    log.info(s"Migrated ${listBlockId.size} blocks in $duration seconds")
  }

  def main(args: Array[String]): Unit = {
    println("------------Start-------------")

    val oldSettingsFilename = "dbtools/src/main/resources/oldData.conf"
    val oldSettings: AppSettings = AppSettings.read(StartupOpts(Some(oldSettingsFilename), None))

    val newSettingsFilename = "dbtools/src/main/resources/newData.conf"
    val newSettings: AppSettings = AppSettings.read(StartupOpts(Some(newSettingsFilename), None))

    migrate(oldSettings, newSettings)
  }
}
