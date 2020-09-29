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
import scala.util.Try
import java.io.File
import java.io.PrintWriter

import scala.io.Source

object DBMigration extends Logging {

  private def getIds(oldSettings: AppSettings): Unit = {
    /* If new-data data exists, remove before creating new-data */
    val path: Path = Path(".bifrost/blockIds/bids.txt")
    Try(path.deleteRecursively())

    var idList: ArrayBuffer[String] = ArrayBuffer[String]()
    val oldHistory: History = History.readOrGenerate(oldSettings)
    var blockId: ModifierId = oldHistory.bestBlockId
    var height: Long = oldHistory.height
    val writer = new PrintWriter(new File(".bifrost/blockIds/bids.txt"))
    val start = System.nanoTime()
    log.info(s"Blockchain height is ${height}, blockId: ${Base58.encode(blockId.hashBytes)}")

    while(!(blockId.hashBytes sameElements History.GenesisParentId)) {
      val currentBlock: Block = oldHistory.storage.storage.get(ByteArrayWrapper(blockId.hashBytes)).map { bw =>
        val bytes = bw.data
        BlockSerializer.decode(bytes.tail).get
      }.get

      idList += Base58.encode(blockId.hashBytes)

      println(s"------${height}---------${Base58.encode(blockId.hashBytes)}------${Base58.encode(currentBlock.serializedId)}----BlockParentId:${Base58.encode(currentBlock.parentId.hashBytes)}")
      blockId = currentBlock.parentId

      height = height - 1
    }

    for (bid <- idList.reverse) writer.write(s"$bid\n")
    writer.close()
    val duration = (System.nanoTime() - start) / 1e9d
    log.info(s"Found all blockIds in $duration seconds")
  }

  private def migrate(oldSettings: AppSettings, newSettings: AppSettings): Unit = {
    val oldHistory: History = History.readOrGenerate(oldSettings)
    val newHistory: History = History.readOrGenerate(newSettings)
    var newState: State = State.readOrGenerate(newSettings, callFromGenesis = true, newHistory)
    var height: Long = oldHistory.height
    val start = System.nanoTime()

    height = 1
    var parentBlockId: ModifierId = ModifierId(History.GenesisParentId)

    val idSource = Source.fromFile(".bifrost/blockIds/bids.txt")

    idSource.getLines.foreach{ line =>
      val bid: Array[Byte] = Base58.decode(line).get
      val currentBlock: Block = oldHistory.storage.storage.get(ByteArrayWrapper(bid)).map { bw =>
        val bytes = bw.data
        BlockSerializer.decode(bytes.tail).get
      }.get.copy(parentId = parentBlockId)
      val currentDifficulty: Long = oldHistory.storage.storage.get(ByteArrayWrapper(bid)).map { bw =>
        val bytes = bw.data
        Longs.fromByteArray(bytes)
      }.get
      println(s"Height:$height----BlockId:${Base58.encode(bid)}----BlockSerializedId:${Base58.encode(currentBlock.serializedId)}----BlockParentId:${Base58.encode(currentBlock.parentId.hashBytes)}----Difficulty:$currentDifficulty")
      height = height + 1
      newHistory.storage.update(currentBlock, currentDifficulty, isBest = true)
      newState = newState.applyModifier(currentBlock).get
      parentBlockId = currentBlock.id
    }

    idSource.close()
    val duration = (System.nanoTime() - start) / 1e9d
    log.info(s"Migrated blocks in $duration seconds")
    height -= 1

    /* Compare data in history */
//    var oldBlockId = ModifierId(Base58.decode("F8WXWNWeFFvDP7ii4d3QEn4wRe1ZrsajmhrGEtvA9mG6").get)
//    var newBlockId = newHistory.bestBlockId
//    println(s"----new:${Base58.encode(newHistory.bestBlockId.hashBytes)}")
//    while(!(oldBlockId.hashBytes sameElements History.GenesisParentId) && height > 2500) {
//      val oldBlock: Block = oldHistory.storage.storage.get(ByteArrayWrapper(oldBlockId.hashBytes)).map { bw =>
//        val bytes = bw.data
//        BlockSerializer.decode(bytes.tail).get
//      }.get
//
//      val newBlock: Block = newHistory.storage.modifierById(newBlockId).get
//
//      println(s"\n\n------${height}---------${Base58.encode(oldBlock.serializedId)}------${Base58.encode(newBlock.serializedId)}")
//      oldBlockId = oldBlock.parentId
//      newBlockId = newBlock.parentId
//
//      println("================================================")
//      println(oldBlock.json)
//      println("----------------")
//      println(newBlock.json)
//      println("================================================")
//
//      height = height - 1
//    }
  }

  def main(args: Array[String]): Unit = {
    println("------------Start-------------")

    val oldSettingsFilename = "dbtools/src/main/resources/oldData.conf"
    val oldSettings: AppSettings = AppSettings.read(StartupOpts(Some(oldSettingsFilename), None))

    val newSettingsFilename = "dbtools/src/main/resources/newData.conf"
    val newSettings: AppSettings = AppSettings.read(StartupOpts(Some(newSettingsFilename), None))

    /* If new-data data exists, remove before creating new-data */
    val path: Path = Path(".bifrost/new-data")
    Try(path.deleteRecursively())

//    getIds(oldSettings)
    migrate(oldSettings, newSettings)
  }
}
