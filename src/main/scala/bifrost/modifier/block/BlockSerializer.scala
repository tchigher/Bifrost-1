package bifrost.modifier.block

import bifrost.crypto.Signature25519
import bifrost.crypto.serialization.Signature25519Serializer
import bifrost.modifier.ModifierId
import bifrost.modifier.box.ArbitBox
import bifrost.modifier.box.serialization.BoxSerializer
import bifrost.modifier.transaction.bifrostTransaction.Transaction
import bifrost.modifier.transaction.serialization.TransactionSerializer
import bifrost.utils.Extensions._
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}

import bifrost.utils.bytesToId
import com.google.common.primitives.{Ints, Longs}
import scala.util.Try
import scala.annotation.tailrec

object BlockSerializer extends BifrostSerializer[Block] {

  override def serialize(block: Block, w: Writer): Unit = {
    /* version: Byte */
    w.put(block.version)

    /* parentId: ModifierId */
    w.putBytes(block.parentId.hashBytes)

    /* timestamp: Long */
    w.putULong(block.timestamp)

    /* generatorBox: ArbitBox */
    BoxSerializer.serialize(block.forgerBox, w)

    /* signature: Signature25519 */
    Signature25519Serializer.serialize(block.signature, w)

    /* txsLength: Int */
    w.putUInt(block.txs.length)

    /* txs: Seq[Transaction] */
    block.txs.foreach(tx => TransactionSerializer.serialize(tx, w))
  }

  override def parse(r: Reader): Block = {
    /* The order of the getByte, getLong... calls should not be changed */

    // TODO: Jing - Version should be used in the future to determine if we need additional procedures in parsing
    val version: Byte = r.getByte()

    // TODO: Jing - maybe we could check that the size of bytes to read in reader is less or equal to the max size of a block

    val parentId: ModifierId = ModifierId(r.getBytes(Block.blockIdLength))
    val timestamp: Long = r.getULong()

    // TODO: Jing - scorex uses toIntExact to make sure the Long does not exceed the length of an Int

    val generatorBox: ArbitBox = BoxSerializer.parse(r).asInstanceOf[ArbitBox]

    val signature: Signature25519 = Signature25519Serializer.parse(r)

    val txsLength: Int = r.getUInt().toIntExact
    val txs: Seq[Transaction] = (0 until txsLength).map(_ => TransactionSerializer.parse(r))

    Block(parentId, timestamp, generatorBox, signature, txs, version)
  }


  // TODO: Jing - remove
  def decode(bytes: Array[Byte]): Try[Block] = Try {

    val parentId = bytesToId(bytes.slice(0, Block.blockIdLength))

    val Array(timestamp: Long, generatorBoxLen: Long) = (0 until 2).map {
      i => Longs.fromByteArray(bytes.slice(Block.blockIdLength + i*Longs.BYTES, Block.blockIdLength + (i + 1)*Longs.BYTES))
    }.toArray

    val version = bytes.slice(Block.blockIdLength + 2*Longs.BYTES, Block.blockIdLength + 2*Longs.BYTES + 1).head

    var numBytesRead = Block.blockIdLength + Longs.BYTES*2 + 1

    val generatorBox = BoxSerializer.decode(bytes.slice(numBytesRead, numBytesRead + generatorBoxLen.toInt)).get.asInstanceOf[ArbitBox]

    val signature = Signature25519(bytes.slice(numBytesRead + generatorBoxLen.toInt,
      numBytesRead + generatorBoxLen.toInt + Signature25519.SignatureSize))

    numBytesRead += generatorBoxLen.toInt + Signature25519.SignatureSize

    val numTxExpected = Ints.fromByteArray(bytes.slice(numBytesRead, numBytesRead + Ints.BYTES))
    numBytesRead += Ints.BYTES

    require(numTxExpected >= 0)

    def unfoldLeft[A,B](seed: B)(f: B => Option[(B, A)]): Seq[A] = {
      @tailrec
      def loop(seed: B)(ls: Seq[A]): Seq[A] = f(seed) match {
        case Some((b, a)) => loop(b)(a +: ls)
        case None => ls
      }
      loop(seed)(Nil)
    }.reverse

    val txBytes: Array[Byte] = bytes.slice(numBytesRead, bytes.length)

    val txByteSeq: Seq[Array[Byte]] = unfoldLeft(txBytes) {
      case b if b.length < Ints.BYTES => None
      case b =>
        val bytesToGrab = Ints.fromByteArray(b.take(Ints.BYTES))

        require(bytesToGrab >= 0)

        if (b.length - Ints.BYTES < bytesToGrab) {
          None // we're done because we can't grab the number of bytes required
        } else {
          val thisTx: Array[Byte] = b.slice(Ints.BYTES, Ints.BYTES + bytesToGrab)
          Some((b.slice(Ints.BYTES + bytesToGrab, b.length), thisTx))
        }
    }.ensuring(_.length == numTxExpected)

    val tx: Seq[Transaction] = txByteSeq.map(tx => TransactionSerializer.decode(tx).get)

    Block(parentId, timestamp, generatorBox, signature, tx, version)
  }


  def decode2xAndBefore(bytes: Array[Byte]): Try[Block] = Try {
    val parentId = bytesToId(bytes.slice(0, Block.blockIdLength))

    val Array(timestamp: Long, generatorBoxLen: Long) = (0 until 2).map {
      i => Longs.fromByteArray(bytes.slice(Block.blockIdLength + i * Longs.BYTES, Block.blockIdLength + (i + 1) * Longs.BYTES))
    }.toArray

    val version = bytes.slice(Block.blockIdLength + 2*Longs.BYTES, Block.blockIdLength + 2*Longs.BYTES + 1).head

    var numBytesRead = Block.blockIdLength + Longs.BYTES * 2 + 1

    val generatorBox = BoxSerializer.decode(bytes.slice(numBytesRead, numBytesRead + generatorBoxLen.toInt)).get.asInstanceOf[ArbitBox]
    val signature = Signature25519(bytes.slice(numBytesRead + generatorBoxLen.toInt, numBytesRead + generatorBoxLen.toInt + Signature25519.SignatureSize))

    numBytesRead += generatorBoxLen.toInt + Signature25519.SignatureSize

    val numTxExpected = Ints.fromByteArray(bytes.slice(numBytesRead, numBytesRead + Ints.BYTES))
    numBytesRead += Ints.BYTES

    require(numTxExpected >= 0)

    def unfoldLeft[A,B](seed: B)(f: B => Option[(B, A)]): Seq[A] = {
      @tailrec
      def loop(seed: B)(ls: Seq[A]): Seq[A] = f(seed) match {
        case Some((b, a)) => loop(b)(a +: ls)
        case None => ls
      }
      loop(seed)(Nil)
    }.reverse

    val txBytes: Array[Byte] = bytes.slice(numBytesRead, bytes.length)

    val txByteSeq: Seq[Array[Byte]] = unfoldLeft(txBytes) {
      case b if b.length < Ints.BYTES => None
      case b =>
        val bytesToGrab = Ints.fromByteArray(b.take(Ints.BYTES))

        require(bytesToGrab >= 0)

        if (b.length - Ints.BYTES < bytesToGrab) {
          None // we're done because we can't grab the number of bytes required
        } else {
          val thisTx: Array[Byte] = b.slice(Ints.BYTES, Ints.BYTES + bytesToGrab)
          Some((b.slice(Ints.BYTES + bytesToGrab, b.length), thisTx))
        }
    }.ensuring(_.length == numTxExpected)

    val tx: Seq[Transaction] = txByteSeq.map(tx => TransactionSerializer.decode(tx).get)

    Block(parentId, timestamp, generatorBox, signature, tx, version)
  }
}
