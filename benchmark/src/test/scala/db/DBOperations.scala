package db

import java.util.concurrent.TimeUnit

import bifrost.modifier.ModifierId
import org.openjdk.jmh.annotations._
import bifrost.BifrostGenerators
import bifrost.history._
import bifrost.modifier.block.{Block, BlockSerializer}
import io.iohk.iodb.ByteArrayWrapper


@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 2)
@Measurement(iterations = 10)
@State(Scope.Benchmark)
class DBOperations extends BifrostGenerators {
  var history: History = generateHistory
  val numOfBlocks: Int = 550
  val numLastBlocks: Int = 500

  val listBlockId: List[ByteArrayWrapper] = (for (_ <- 1 to numOfBlocks) yield {
    val oneBlock: Block = BlockGen.sample.get.copy(parentId = history.bestBlockId)
    history = history.append(oneBlock).get._1
    /* println(s"forging====$i====${ByteArrayWrapper(oneBlock.id)}") */
    ByteArrayWrapper(oneBlock.id.hashBytes)
  }).take(numLastBlocks).toList

  val bestBlockIdKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(history.storage.storage.keySize)(-1: Byte))
  val storageCurBlockId: ModifierId = ModifierId(history.storage.storage.get(bestBlockIdKey).get.data)
  val cacheCurBlockId: ModifierId = ModifierId(history.storage.storage.get(bestBlockIdKey).get.data)


  /* Read from storage */
  @Benchmark
  def storageTest() {
    var tmpStorageBlockId: ModifierId = storageCurBlockId
    for (_ <- 1 to numLastBlocks) {
      val currentBlock: Block = history.storage.storage.get(ByteArrayWrapper(tmpStorageBlockId.hashBytes)).map { bw =>
        val bytes = bw.data
        BlockSerializer.parseBytes(bytes.tail).get
      }.get
      tmpStorageBlockId = currentBlock.parentId
    }
  }

  /* Read from cache */
  @Benchmark
  def cacheTest() {
    var tmpCacheBlockId: ModifierId = cacheCurBlockId
    for (_ <- 1 to numLastBlocks) {
      val currentBlock: Block = history.storage.blockCache.getIfPresent(ByteArrayWrapper(tmpCacheBlockId.hashBytes)).map {
        bw =>
          val bytes = bw.data
          BlockSerializer.parseBytes(bytes.tail).get
      }.get
      tmpCacheBlockId = currentBlock.parentId
    }
  }

  /* Testing only accessing the storage, not parsing the serialized data */
  @Benchmark
  def storageRead() {
    for (id <- listBlockId) {
      val smt = history.storage.storage.get(id)
    }
  }

  /* Testing only accessing the cache, not parsing the serialized data */
  @Benchmark
  def cacheRead() {
    for (id <- listBlockId) {
      val smt = history.storage.blockCache.getIfPresent(id)
    }
  }
}


/*
package db

import java.io.File
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicLong

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import bifrost.history._
import io.circe.Json
import io.iohk.iodb.LSMStore


@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@State(Scope.Thread)
class DBOperations {

  private val value = new AtomicLong()

  @Benchmark
  def test(bh: Blackhole): Unit = {
    bh.consume(value.addAndGet(42))
  }

  @Benchmark
  def readBlock(bh: Blackhole): Unit = {

  }
}

object DBOperations {



  @State(Scope.Benchmark)
  class baseState {

    //TODO Replace hardcoded dir with one from settings
    val settings: ForgingSettings = new ForgingSettings {
      override def settingsJSON: Map[String, Json] = settingsFromFile("bench.json")
    }
    val dataDir: File = new File(settings.dataDirOpt.get)
    println(settings.dataDirOpt.getOrElse("Could not find bench.json"))
    val blockStorage: LSMStore = new LSMStore(dataDir)
  }
}
 */