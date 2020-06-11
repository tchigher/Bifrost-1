package bifrost.modifier.transaction.serialization

import bifrost.modifier.transaction.bifrostTransaction.mArbitTransfer
import bifrost.serialization.Serializer
import com.google.common.primitives.Ints

import scala.util.Try

object mArbitTransferCompanion extends Serializer[mArbitTransfer] with mTransferSerializer {

  override def toBytes(ac: mArbitTransfer): Array[Byte] = {
    TransferTransactionCompanion.prefixBytes ++ toChildBytes(ac)
  }

  def toChildBytes(ac: mArbitTransfer): Array[Byte] = {
    transferToBytes(ac, "ArbitTransfer") ++
      ac.data.getBytes++
      Ints.toByteArray(ac.data.getBytes.length)
  }

  override def parseBytes(bytes: Array[Byte]): Try[mArbitTransfer] = Try {
    val params = parametersParseBytes(bytes)
    val dataLen: Int = Ints.fromByteArray(bytes.slice(bytes.length - Ints.BYTES, bytes.length))
    val data: String = new String(
      bytes.slice(bytes.length - Ints.BYTES - dataLen, bytes.length - Ints.BYTES)
    )
    mArbitTransfer(params._1, params._2, params._3, params._4, params._5, data)
  }
}
